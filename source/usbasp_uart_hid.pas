unit usbasp_uart_hid;

{

  This file is part of
    Nephelae USBasp HID UART.

  HIDAPI Communications.

  Copyright (C) 2021 - 2022 Dimitrios Chr. Ioannidis.
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
  Classes, SysUtils, hidapi, fgl;

type

  PUSBaspHIDDevice = ^TUSBaspHIDDevice;

  TUSBaspHIDDevice = record
    HidDevice: PHidDevice;
    Path: string;
    Serial: string;
    Manufacturer: string;
    Product: string;
    FirmwareVersion: string;
    Capabilities: string;
    VendorID: word;
    ProductID: word;
    InterfaceNumber: integer;
    ReportSize: word;
    PacketCount: byte;
  end;

  TUSBaspHIDDeviceList = specialize TFPGList<PUSBaspHIDDevice>;

function usbasp_hid_enumerate(AUSBaspHIDDeviceList: TUSBaspHIDDeviceList): integer;
procedure usbasp_hid_open(const AUSBaspHIDDevice: PUSBaspHIDDevice);
procedure usbasp_hid_close;
function usbasp_hid_read(var Data): integer;
function usbasp_hid_write(var Data): integer;
function usbasp_hid_uart_set_conf(var Data): integer;
function usbasp_hid_uart_get_conf(var Data): integer;

var
  USBaspHIDList: TUSBaspHIDDeviceList;

implementation

uses
  Math, StrUtils, uusbaspdefinitions;

var
  HidBuffer: array[0..8] of byte;
  USBaspHIDDevice: PUSBaspHIDDevice;
  i: integer;

function usbasp_hid_enumerate(AUSBaspHIDDeviceList: TUSBaspHIDDeviceList): integer;
var
  HidEnumerateList, HidItem: PHidDeviceInfo;
  USBaspHIDDevice: PUSBaspHIDDevice;
begin
  try
    HidEnumerateList := THidDeviceInfo.Enumerate(USBASP_SHARED_VID, USBASP_SHARED_PID);
    HidItem := HidEnumerateList;
    while Assigned(HidItem) do
    begin
      New(USBaspHIDDevice);
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
      AUSBaspHIDDeviceList.Add(USBaspHIDDevice);
      HidItem := HidItem^.Next;
    end;
  finally
    HidEnumerateList^.Free;
  end;
  Result := AUSBaspHIDDeviceList.Count;
end;

//procedure usbasp_enumerate(const APrintInfo: boolean);
//var
//  USBasp: PUSBaspHIDDevice;
//begin
//  WriteLn();
//  WriteLn('Enumerating USBasp HIDUART (Hid Api Library Version : ', HidApiVersion, ' )');
//
//  if enumerate_hid(USBaspHIDList) > 0 then
//  begin
//    for USBasp in USBaspHIDList do
//    begin
//      usbasp_open(USBasp);
//      usbasp_close;
//      if APrintInfo then
//        PrintInfo(USBasp);
//    end;
//    //ReadLn();
//  end
//  else
//    WriteLn('No USBasp HID UART found.');
//end;

procedure usbasp_hid_open(const AUSBaspHIDDevice: PUSBaspHIDDevice);
begin
  USBaspHIDDevice := AUSBaspHIDDevice;
  USBaspHIDDevice^.HidDevice := THidDevice.OpenPath(AUSBaspHIDDevice^.Path);
end;

procedure usbasp_hid_close;
begin
  USBaspHIDDevice^.HidDevice^.Close;
  USBaspHIDDevice := nil;
end;

function usbasp_hid_read(var Data): integer;
var
  HidSize: SizeInt;
begin
  // Report size plus added Report ID
  HidSize := USBaspHIDDevice^.HidDevice^.ReadTimeout(HidBuffer, USBaspHIDDevice^.ReportSize + 1, 20);

  Move(HidBuffer, Data, HidSize);
  Result := HidSize;
end;

function usbasp_hid_write(var Data): integer;
var
  HidSize: SizeInt;
begin
  //  // Add Report ID
  HidBuffer[0] := $00;
  Move(Data, HidBuffer[1], USBaspHIDDevice^.ReportSize + 1);

  // Report size plus added Report ID
  HidSize := USBaspHIDDevice^.HidDevice^.Write(HidBuffer, USBaspHIDDevice^.ReportSize + 1);

  Result := HidSize;
end;

function usbasp_hid_uart_get_conf(var Data): integer;
var
  HidSize: SizeInt;
begin
  //  // Add Report ID
  HidBuffer[0] := $00;
  //Move(Data, HidBuffer[1], USBaspHIDDevice^.ReportSize + 1);

  // Report size plus added Report ID
  HidSize := USBaspHIDDevice^.HidDevice^.GetFeatureReport(HidBuffer, USBaspHIDDevice^.ReportSize + 1) - 1;

  Move(HidBuffer[1], Data, HidSize);

  Result := HidSize - 1;
end;

function usbasp_hid_uart_set_conf(var Data): integer;
var
  HidSize: SizeInt;
begin
  //  // Add Report ID
  HidBuffer[0] := $00;
  Move(Data, HidBuffer[1], USBaspHIDDevice^.ReportSize + 1);

  // Report size plus added Report ID
  HidSize := USBaspHIDDevice^.HidDevice^.SendFeatureReport(HidBuffer, USBaspHIDDevice^.ReportSize + 1);

  Move(HidBuffer, Data, HidSize);

  Result := HidSize;
end;

initialization
  USBaspHIDList := TUSBaspHIDDeviceList.Create;

finalization;
  i := USBaspHIDList.Count - 1;
  while i >= 0 do
  begin
    Dispose(USBaspHIDList[i]);
    Dec(i);
  end;
  USBaspHIDList.Free;

end.
