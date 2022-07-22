unit USBasp;

{

  This file is part of Nephelae's Object Pascal FPUSBasp.

  USBasp Class.

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SPSCRingBuffer,
  USBasp_Definitions,
  USBasp_LIBUSB,
  USBasp_HIDAPI,
  USBasp_Threads;

const
  TUARTBaudRate: array[0..13] of integer =
    (300, 600, 1200, 2400, 4800, 9600, 14400, 19200, 28800, 31250, 38400,
    57600, 74880, 115200);

  TUARTDataBits: array[0..4] of integer =
    (USBASP_UART_BYTES_5B, USBASP_UART_BYTES_6B, USBASP_UART_BYTES_7B,
    USBASP_UART_BYTES_8B, USBASP_UART_BYTES_9B);
  TUARTParity: array[0..2] of integer =
    (USBASP_UART_PARITY_NONE, USBASP_UART_PARITY_EVEN, USBASP_UART_PARITY_ODD);
  TUARTStopBit: array[0..1] of integer = (USBASP_UART_STOP_1BIT, USBASP_UART_STOP_2BIT);

  USBaspIDNotFound = 255;

type

  { TUSBasp }

  TUSBasp = class(TObject)
  private
    FUSBaspID: byte;
    FUSBDevices: TUSBasp_USBDeviceList;
    FHidDevices: TUSBasp_HIDDeviceList;
    FUSBaspDeviceSelected: TUSBasp_USBDevice;
    FConnected: boolean;
    FUARTOpened: boolean;
    FLastUsbError: integer;
    FReceiveBuffer, FTransmitBuffer, FMonitorBuffer: TSPSCRingBuffer;
    FReceiveThread: TThreadHID_UARTRead;
    FTransmitThread: TThreadHID_UARTWrite;
    FMonitorThread: TThreadHID_Read;
    procedure SetUSBaspID(const AValue: byte);
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AUSBaspDeviceID: byte = 0): boolean;
    function Disconnect: boolean;

    function UARTOpen(const ABaudRate, ADataBits, AParity, AStopBits: integer): boolean;
    function UARTClose: boolean;

    function EnumerateDevices: integer;

    property Connected: boolean read FConnected;
    property UARTOpened: boolean read FUARTOpened;

    property ReceiveBuffer: TSPSCRingBuffer read FReceiveBuffer;
    property TransmitBuffer: TSPSCRingBuffer read FTransmitBuffer;
    property MonitorBuffer: TSPSCRingBuffer read FMonitorBuffer;

    property USBaspID: byte read FUSBaspID write SetUSBaspID;
    property USBaspDevice: TUSBasp_USBDevice read FUSBaspDeviceSelected;
    property USBaspDevices: TUSBasp_USBDeviceList read FUSBDevices;
  end;

implementation

{ TFPUSBasp }

constructor TUSBasp.Create;
begin
  usbasp_libusb_initialization;

  FUSBDevices := TUSBasp_USBDeviceList.Create;
  FHidDevices := TUSBasp_HIDDeviceList.Create;

  FReceiveBuffer := TSPSCRingBuffer.Create(8192);
  FTransmitBuffer := TSPSCRingBuffer.Create(8192);
  FMonitorBuffer := TSPSCRingBuffer.Create(64);

  FUSBaspID := USBaspIDNotFound;
  FConnected := False;
  FUARTOpened := False;
end;

destructor TUSBasp.Destroy;
begin
  UARTClose();
  Disconnect;

  FreeAndNil(FReceiveBuffer);
  FreeAndNil(FTransmitBuffer);
  FreeAndNil(FMonitorBuffer);

  FHidDevices.Free;
  FUSBDevices.Free;

  usbasp_libusb_finalization;

  inherited Destroy;
end;

function TUSBasp.Connect(const AUSBaspDeviceID: byte = 0): boolean;
begin
  if (not FConnected) and (FUSBDevices.Count > 0) and
    (AUSBaspDeviceID in [0..(FUSBDevices.Count - 1)]) then
  begin
    FUSBaspID := AUSBaspDeviceID;
    FUSBaspDeviceSelected := FUSBDevices[FUSBaspID]^;
    if FUSBaspDeviceSelected.HasHIDUart and
      Assigned(FUSBDevices[FUSBaspID]^.MonitorHidDevice) then
    begin
      usbasp_hidapi_open(FUSBDevices[FUSBaspID]^.MonitorHidDevice);
      FMonitorThread := TThreadHID_Read.Create(FUSBDevices[FUSBaspID]^.MonitorHidDevice,
        FMonitorBuffer);
    end;
    if FLastUsbError < 0 then
    begin

    end
    else
      FConnected := True;
  end;
  Result := FConnected;
end;

function TUSBasp.Disconnect: boolean;
begin
  if FConnected then
  begin
    if FUARTOpened then
      UARTClose;

    //usbasp_hidapi_close(FUSBDevices[FUSBaspID]^.HidDevice);

    if Assigned(FUSBDevices[FUSBaspID]^.MonitorHidDevice) then
    begin
      FMonitorThread.Terminate;
      FMonitorThread.WaitFor;
      FreeAndNil(FMonitorThread);

      usbasp_hidapi_close(FUSBDevices[FUSBaspID]^.MonitorHidDevice);
    end;

    if FLastUsbError < 0 then
    begin

    end
    else
      FConnected := False;
  end;
  Result := FConnected;
end;

function TUSBasp.UARTOpen(const ABaudRate, ADataBits, AParity, AStopBits:
  integer): boolean;
var
  Buffer: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
  Prescaler: word;
begin
  if FConnected and (FUSBaspDeviceSelected.HasUart or
    FUSBaspDeviceSelected.HasHIDUart) and not FUARTOpened then
  begin
    usbasp_hidapi_open(FUSBDevices[FUSBaspID]^.HidDevice);

    Prescaler := FUSBDevices[FUSBaspID]^.CrystalOsc div 8 div ABaudRate - 1;

    Buffer[0] := lo(Prescaler);
    Buffer[1] := hi(Prescaler);
    Buffer[2] := ADataBits or AStopBits or AParity;

    FReceiveThread := TThreadHID_UARTRead.Create(FUSBDevices[FUSBaspID]^.HidDevice,
      FReceiveBuffer);
    FTransmitThread := TThreadHID_UARTWrite.Create(FUSBDevices[FUSBaspID]^.HidDevice,
      FTransmitBuffer);

    usbasp_hidapi_uart_set_conf(FUSBDevices[FUSBaspID]^.HidDevice, Buffer);

    FUARTOpened := True;

  end;
  Result := FUARTOpened;
end;

function TUSBasp.UARTClose: boolean;
var
  Buffer: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
begin
  if FConnected and (FUSBaspDeviceSelected.HasUart or
    FUSBaspDeviceSelected.HasHIDUart) and FUARTOpened then
  begin

    Buffer[0] := 0;
    Buffer[1] := 0;
    Buffer[2] := 0;

    usbasp_hidapi_uart_set_conf(FUSBDevices[FUSBaspID]^.HidDevice, Buffer);

    Sleep(100);

    FReceiveThread.Terminate;
    FReceiveThread.WaitFor;
    FreeAndNil(FReceiveThread);

    FTransmitThread.Terminate;
    FTransmitThread.WaitFor;
    FreeAndNil(FTransmitThread);

    usbasp_hidapi_close(FUSBDevices[FUSBaspID]^.HidDevice);

    FUARTOpened := False;
  end;
  Result := FUARTOpened;
end;

procedure TUSBasp.SetUSBaspID(const AValue: byte);
begin
  if FUSBaspID = AValue then
    Exit;
  if FConnected or (FUSBDevices.Count = 0) then
    Exit;
  if AValue in [0..(FUSBDevices.Count - 1)] then
  begin
    FUSBaspID := AValue;
    FUSBaspDeviceSelected := FUSBDevices[FUSBaspID]^;
  end;
end;

function TUSBasp.EnumerateDevices: integer;
var
  USBDevice: PUSBasp_USBDevice;
  HIDDevice: PUSBasp_HIDDevice;
begin
  Result := 0;
  if not FConnected then
  begin
    Result := usbasp_libusb_devices(FUSBDevices);
    usbasp_hidapi_devices(FHidDevices);
    for USBDevice in FUSBDevices do
      for HidDevice in FHidDevices do
      begin
        if IsEqualGUID(HIDDevice^.ContainerID, USBDevice^.ContainerID) then
          case HIDDevice^.InterfaceNumber of
            1: USBDevice^.HidDevice := HidDevice;
            2: USBDevice^.MonitorHidDevice := HIDDevice;
          end;
      end;
  end;
  if Result = 0 then
    FUSBaspID := USBaspIDNotFound;
end;

end.
