unit USBasp_HIDAPI;

{

  This file is part of Nephelae's Object Pascal FPUSBasp.

  HIDAPI Communications.

  Copyright (C) 2022 Dimitrios Chr. Ioannidis.
    Nephelae - https://www.nephelae.eu

  https://www.nephelae.eu/

  Licensed under the MIT License (MIT).
  See licence file in root directory.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
  ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
  TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
  PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
  SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
  ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.

}

{$mode ObjFPC}

interface

uses
  USBasp_Definitions, hidapi;

function usbasp_hidapi_devices(var AUSBaspHIDDeviceList: TUSBasp_HIDDeviceList): integer;
procedure usbasp_hidapi_open(const AUSBaspHIDDevice: PUSBasp_HIDDevice);
procedure usbasp_hidapi_close(const AUSBaspHIDDevice: PUSBasp_HIDDevice);
function usbasp_hidapi_read(const AUSBaspHIDDevice: PUSBasp_HIDDevice; out Data): integer;
function usbasp_hidapi_write(const AUSBaspHIDDevice: PUSBasp_HIDDevice; const Data): integer;
function usbasp_hidapi_uart_set_conf(const AUSBaspHIDDevice: PUSBasp_HIDDevice; const Data): integer;
function usbasp_hidapi_uart_get_conf(const AUSBaspHIDDevice: PUSBasp_HIDDevice; out Data): integer;

implementation

uses
  SysUtils, StrUtils;


function usbasp_hidapi_devices(var AUSBaspHIDDeviceList: TUSBasp_HIDDeviceList): integer;
var
  HidEnumerateList, HidItem: PHidDeviceInfo;
  USBaspHIDDevice: PUSBasp_HIDDevice = nil;
  tmpHidDevice: PHidDevice;
  index: byte;
begin
  AUSBaspHIDDeviceList.FreeItems;
  AUSBaspHIDDeviceList.Clear;
  try
    HidEnumerateList := THidDeviceInfo.Enumerate(USBASP_SHARED_VID, USBASP_SHARED_PID);
    HidItem := HidEnumerateList;
    index := 0;
    while Assigned(HidItem) do
    begin
      New(USBaspHIDDevice);
      USBaspHIDDevice^.index := index;
      USBaspHIDDevice^.Path := HidItem^.Path;
      USBaspHIDDevice^.Serial := PCWCharToUnicodeString(HidItem^.SerialNumber);
      USBaspHIDDevice^.Manufacturer :=
        PCWCharToUnicodeString(HidItem^.ManufacturerString);
      USBaspHIDDevice^.Product := PCWCharToUnicodeString(HidItem^.ProductString);
      USBaspHIDDevice^.InterfaceNumber := HidItem^.InterfaceNumber;
      USBaspHIDDevice^.VendorID := HidItem^.VendorID;
      USBaspHIDDevice^.ProductID := HidItem^.ProductID;
      USBaspHIDDevice^.FirmwareVersion :=
        ReverseString(ReverseString(BCDToInt(HidItem^.ReleaseNumber).ToString()).Insert(2, '.'));
      USBaspHIDDevice^.Capabilities := '';
      USBaspHIDDevice^.ReportSize := 8;
      USBaspHIDDevice^.PacketCount := 1;

      tmpHidDevice := THidDevice.OpenPath(HidItem^.Path);
      USBaspHIDDevice^.ContainerID := tmpHidDevice^.GetContainerID;
      tmpHidDevice^.Close;

      tmpHidDevice := nil;

      AUSBaspHIDDeviceList.Add(USBaspHIDDevice);
      HidItem := HidItem^.Next;
      Inc(Index);
    end;
  finally
    HidEnumerateList^.Free;
  end;
  Result := AUSBaspHIDDeviceList.Count;
end;

procedure usbasp_hidapi_open(const AUSBaspHIDDevice: PUSBasp_HIDDevice);
begin
  AUSBaspHIDDevice^.Device := THidDevice.OpenPath(AUSBaspHIDDevice^.Path);
end;

procedure usbasp_hidapi_close(const AUSBaspHIDDevice: PUSBasp_HIDDevice);
begin
  AUSBaspHIDDevice^.Device^.Close;
end;

function usbasp_hidapi_read(const AUSBaspHIDDevice: PUSBasp_HIDDevice; out Data): integer;
var
  HidBuffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
begin
  // Read default 0 Report ID
  Result := AUSBaspHIDDevice^.Device^.ReadTimeout(HidBuffer, AUSBaspHIDDevice^.ReportSize, 250);
  Move(HidBuffer, Data, AUSBaspHIDDevice^.ReportSize);
end;

function usbasp_hidapi_write(const AUSBaspHIDDevice: PUSBasp_HIDDevice; const Data): integer;
var
  HidBuffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
begin
  // Add Report ID
  HidBuffer[0] := $00;
  Move(Data, HidBuffer[1], AUSBaspHIDDevice^.ReportSize + 1);

  // Report size plus added Report ID
  Result := AUSBaspHIDDevice^.Device^.Write(HidBuffer, AUSBaspHIDDevice^.ReportSize + 1) - 1;
end;

function usbasp_hidapi_uart_get_conf(const AUSBaspHIDDevice: PUSBasp_HIDDevice; out Data): integer;
var
  HidBuffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  HidSize: SizeInt;
begin
  // Add Report ID
  HidBuffer[0] := $00;

  // Report size plus added Report ID
  HidSize := AUSBaspHIDDevice^.Device^.GetFeatureReport(HidBuffer, AUSBaspHIDDevice^.ReportSize + 1) - 1;

  Move(HidBuffer[1], Data, HidSize);

  Result := HidSize - 1;
end;

function usbasp_hidapi_uart_set_conf(const AUSBaspHIDDevice: PUSBasp_HIDDevice; const Data): integer;
var
  HidBuffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  HidSize: SizeInt;
begin
  // Add Report ID
  HidBuffer[0] := $00;
  Move(Data, HidBuffer[1], AUSBaspHIDDevice^.ReportSize + 1);

  // Report size plus added Report ID
  HidSize := AUSBaspHIDDevice^.Device^.SendFeatureReport(HidBuffer, AUSBaspHIDDevice^.ReportSize + 1);

  Result := HidSize;
end;

initialization
  HidInit();

finalization;
  HidExit();

end.
