unit uUSBasp;

{

  This file is part of Object Pascal libUSB UART Terminal for USBasp ( Firmware 1.5 patched ).

  libUSB USBasp UART library.

  Copyright (C) 2020 Dimitrios Chr. Ioannidis.
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
  Classes, SysUtils, Forms, StdCtrls, syncobjs, usbasp_uart;

const
  TSerialBaudRate: array[0..13] of integer =
    (300, 600, 1200, 2400, 4800, 9600, 14400, 19200, 28800, 31250, 38400,
    57600, 74880, 115200);

type
  TUARTReceive = procedure(const AMsg: string) of object;

  TWriteBuffer = array of byte;

  TRawSerialDataMsg = record
    AsString: string;
  end;
  PRawSerialDataMsg = ^TRawSerialDataMsg;

  { TUSBasp }

  TUSBasp = class(TObject)
  private
    FUSBaspID: byte;
    FUSBaspDevices: TUSBaspDeviceList;
    FConnected: boolean;
    FUARTOpened: boolean;
    FWriteBuffer: TWriteBuffer;
    FWriteBufferLock: TCriticalSection;
    FLastUsbError: integer;
    FThreadReceive, FThreadSend: TThread;
    FOnUARTReceive: TUARTReceive;
    function GetUSBaspDevice: TUSBaspDevice;
    procedure SetConnected(const AValue: boolean);
    procedure SetOnUARTReceive(const AValue: TUARTReceive);
    procedure SetUSBaspID(const AValue: byte);
  public
    constructor Create;
    destructor Destroy; override;
    function Connect(const AUSBaspDeviceID: byte = 0): boolean;
    function Disconnect: boolean;
    function UARTOpen(const ABaudRate: integer): boolean;
    function UARTClose: boolean;
    procedure LoadUSBaspDevices;
    procedure UARTWrite(const ABuffer: string);
    property OnUARTReceive: TUARTReceive read FOnUARTReceive write SetOnUARTReceive;
    property Connected: boolean read FConnected;
    property UARTOpened: boolean read FUARTOpened;
    property USBaspID: byte read FUSBaspID write SetUSBaspID;
    property USBaspDevice: TUSBaspDevice read GetUSBaspDevice;
    property USBaspDevices: TUSBaspDeviceList read FUSBaspDevices;
    property UARTWriteBuffer: TWriteBuffer read FWriteBuffer;
  end;

implementation

uses
  uUSBasp_Threads;

{ TUSBasp }

constructor TUSBasp.Create;
begin
  usbasp_initialization;
  FWriteBufferLock := TCriticalSection.Create;
  FUSBaspDevices := TUSBaspDeviceList.Create;
  FUSBaspID := 255;
  FConnected := False;
  FWriteBuffer := TWriteBuffer.Create(0);
end;

destructor TUSBasp.Destroy;
begin
  UARTClose;
  Disconnect;
  FUSBaspDevices.Free;
  usbasp_finalization;
  FWriteBufferLock.Free;
  inherited Destroy;
end;

function TUSBasp.Connect(const AUSBaspDeviceID: byte = 0): boolean;
begin
  if (not FConnected) then
  begin
    FLastUsbError := usbasp_open(FUSBaspDevices[AUSBaspDeviceID]);
    if FLastUsbError < 0 then
    begin
      FUSBaspID := AUSBaspDeviceID;
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
    FLastUsbError := usbasp_close(FUSBaspDevices[FUSBaspID]);
    if FLastUsbError < 0 then
    begin

    end
    else
      FConnected := False;
  end;
  Result := FConnected;
end;

function TUSBasp.UARTOpen(const ABaudRate: integer): boolean;
var
  iRealBaud: integer;
begin
  if FConnected and FUSBaspDevices[FUSBaspID]^.HasUart and not FUARTOpened then
  begin
    iRealBaud := ABaudRate;
    FLastUsbError := usbasp_uart_enable(FUSBaspDevices[FUSBaspID]^.Handle,
      iRealBaud, USBASP_UART_BYTES_8B or USBASP_UART_STOP_1BIT or
      USBASP_UART_PARITY_NONE);
    FUARTOpened := True;
    FThreadReceive := TUSBaspReceiveThread.Create(Self);
    FThreadSend := TUSBaspSendThread.Create(Self, FWriteBufferLock);
  end;
  Result := FUARTOpened;
end;

function TUSBasp.UARTClose: boolean;
begin
  if FConnected and FUSBaspDevices[FUSBaspID]^.HasUart and FUARTOpened then
  begin
    FThreadReceive.Terminate;
    FThreadReceive.WaitFor;
    FreeAndNil(FThreadReceive);

    FThreadSend.Terminate;
    FThreadSend.WaitFor;
    FreeAndNil(FThreadSend);

    usbasp_uart_disable(FUSBaspDevices[FUSBaspID]);
    FUARTOpened := False;
  end;
  Result := FUARTOpened;
end;

function TUSBasp.GetUSBaspDevice: TUSBaspDevice;
begin
  Result := FUSBaspDevices[FUSBaspID]^;
end;

procedure TUSBasp.SetConnected(const AValue: boolean);
begin
  if FConnected = AValue then
    Exit;
  FConnected := AValue;
end;

procedure TUSBasp.SetOnUARTReceive(const AValue: TUARTReceive);
begin
  if FOnUARTReceive = AValue then
    Exit;
  FOnUARTReceive := AValue;
end;

procedure TUSBasp.SetUSBaspID(const AValue: byte);
begin
  if FUSBaspID = AValue then
    Exit;
  if FConnected then
    Exit;
  FUSBaspID := AValue;
end;

procedure TUSBasp.LoadUSBaspDevices;
begin
  usbasp_devices(FUSBaspDevices);
  FUSBaspDevices.Count;
end;

procedure TUSBasp.UARTWrite(const ABuffer: string);
var
  tmp: TBytes;
begin
  FWriteBufferLock.Acquire;
  try
    tmp := BytesOf(ABuffer);
    SetLength(FWriteBuffer, Length(tmp) + 1);
    move(tmp[0], FWriteBuffer[1], Length(tmp));
    FWriteBuffer[0] := 1;
  finally
    FWriteBufferLock.Release;
  end;
end;

end.
