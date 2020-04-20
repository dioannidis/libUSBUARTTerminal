unit uUSBasp;

{

  This file is part of Object Pascal libUSB UART Terminal for USBasp ( Firmware 1.5 patched ).

  libUSB USBasp UART Terminal.

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

  TRawSerialDataMsg = record
    AsString: string;
  end;
  PRawSerialDataMsg = ^TRawSerialDataMsg;

  { TUSBasp }

  TUSBasp = class(TObject)
  private
    FConnected: boolean;
    FSupportUART: boolean;
    FSupportTPI: boolean;
    FUARTOpened: boolean;
    FUSBaspID: byte;
    FUSBaspDevices: TUSBaspDeviceList;
    FBufferSend: array of byte;
    FBufferReady: boolean;
    FLastUsbError: integer;
    FThreadReceive, FThreadSend: TThread;
    function GetUSBaspDevice: TUSBaspDevice;
    procedure SetConnected(AValue: boolean);
    procedure SetOnUARTReceive(AValue: TUARTReceive);
    procedure SetSendBuffer(const AValue: string);
    procedure SetUSBaspID(AValue: byte);
  protected
    FOnUARTReceive: TUARTReceive;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AUSBaspDeviceID: byte = 0): boolean;
    function Disconnect: boolean;

    function UARTOpen(const ABaudRate: integer): boolean;
    function UARTClose: boolean;

    procedure LoadUSBaspDevices;

    property USBaspID: byte read FUSBaspID write SetUSBaspID;
    property Connected: boolean read FConnected;
    property UARTOpened: boolean read FUARTOpened;
    property SendBuffer: string write SetSendBuffer;
    property SupportUART: boolean read FSupportUART;
    property USBaspDevices: TUSBaspDeviceList read FUSBaspDevices;
    property USBaspDevice: TUSBaspDevice read GetUSBaspDevice;

    property OnUARTReceive: TUARTReceive read FOnUARTReceive write SetOnUARTReceive;
  end;

implementation

uses
  libusb;

var
  SendLock: TCriticalSection;

type
  { TUSBaspReceiveThread }

  TUSBaspReceiveThread = class(TThread)
  private
    FUSBasp: TUSBasp;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFUSBasp: TUSBasp);
    destructor Destroy; override;
  end;

procedure TUSBaspReceiveThread.Execute;
const
  USBaspBufferSize = 254;
var
  USBaspBuffer: array of byte;
  RcvLen, LastUSBError: integer;
  RawSerialData: TBytes;
begin

  SetLength(USBaspBuffer, USBaspBufferSize);

  while not terminated do
  begin
    // Receive
    RcvLen := usbasp_uart_read(FUSBasp.FUSBaspDevices[FUSBasp.FUSBaspID],
      PChar(@USBaspBuffer[0]), USBaspBufferSize);
    if (RcvLen = 0) then
    begin
      Sleep(1);
      Continue;
    end;
    if RcvLen < 0 then
    begin
      if Assigned(FUSBasp.FOnUARTReceive) then
        FUSBasp.FOnUARTReceive(libusb_strerror(libusb_error(RcvLen)));
      Sleep(20);
    end
    else
    begin
      SetLength(RawSerialData, RcvLen);
      Move(USBaspBuffer[0], RawSerialData[0], RcvLen);
      if Assigned(FUSBasp.FOnUARTReceive) then
        FUSBasp.FOnUARTReceive(TEncoding.ASCII.GetString(RawSerialData));
      USBaspBuffer := nil;
      SetLength(USBaspBuffer, USBaspBufferSize);
    end;
  end;
end;

constructor TUSBaspReceiveThread.Create(const AFUSBasp: TUSBasp);
begin
  FUSBasp := AFUSBasp;
  inherited Create(False);
end;

destructor TUSBaspReceiveThread.Destroy;
begin
  inherited Destroy;
end;

type
  { TUSBaspSendThread }

  TUSBaspSendThread = class(TThread)
  private
    FUSBasp: TUSBasp;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFUSBasp: TUSBasp);
    destructor Destroy; override;
  end;

{ TUSBaspSendThread }

procedure TUSBaspSendThread.Execute;
const
  USBaspBufferSize = 254;
var
  USBaspBuffer: array of byte;
  RcvLen, LastUSBError: integer;
  RawSerialData: TBytes;
begin
  while not terminated do
  begin
    // Send
    SendLock.Acquire;
    try
      if FUSBasp.FBufferReady then
      begin
        RcvLen:=0;
        while RcvLen < Length(FUSBasp.FBufferSend) do
        begin
          RcvLen := usbasp_uart_write(FUSBasp.FUSBaspDevices[FUSBasp.FUSBaspID],
            PChar(@FUSBasp.FBufferSend[0]), Length(FUSBasp.FBufferSend));
          if RcvLen < 0 then
          begin
            if Assigned(FUSBasp.FOnUARTReceive) then
              FUSBasp.FOnUARTReceive(libusb_strerror(libusb_error(RcvLen)));
            Sleep(20);
          end;
        end;
        FUSBasp.FBufferReady := False;
      end;
      Sleep(1);
    finally
      SendLock.Release;
    end;
  end;
end;

constructor TUSBaspSendThread.Create(const AFUSBasp: TUSBasp);
begin
  FUSBasp := AFUSBasp;
  inherited Create(False);
end;

destructor TUSBaspSendThread.Destroy;
begin
  inherited Destroy;
end;

{ TUSBasp }

function TUSBasp.GetUSBaspDevice: TUSBaspDevice;
begin
  Result := FUSBaspDevices[FUSBaspID]^;
end;

procedure TUSBasp.SetConnected(AValue: boolean);
begin
  if FConnected = AValue then
    Exit;
  FConnected := AValue;
end;

procedure TUSBasp.SetOnUARTReceive(AValue: TUARTReceive);
begin
  if FOnUARTReceive = AValue then
    Exit;
  FOnUARTReceive := AValue;
end;

procedure TUSBasp.SetSendBuffer(const AValue: string);
var
  tmp: TBytes;
begin
  if (AValue.Length = 0) then
    Exit;
  SendLock.Acquire;
  try
    tmp := BytesOf(AValue);
    SetLength(FBufferSend, Length(tmp));
    move(tmp[0], FBufferSend[0], Length(tmp));
    FBufferReady := True;
  finally
    SendLock.Release;
  end;
end;

procedure TUSBasp.SetUSBaspID(AValue: byte);
begin
  if FUSBaspID = AValue then
    Exit;
  if FConnected then
    Exit;
  FUSBaspID := AValue;
  FSupportUART := FUSBaspDevices[FUSBaspID]^.HasUart;
  FSupportTPI := FUSBaspDevices[FUSBaspID]^.HasTPI;
end;

procedure TUSBasp.LoadUSBaspDevices;
begin
  usbasp_devices(FUSBaspDevices);
  FUSBaspDevices.Count;
end;


constructor TUSBasp.Create;
begin
  usbasp_initialization;
  FUSBaspDevices := TUSBaspDeviceList.Create;
  FUSBaspID := 255;
  FConnected := False;
  FSupportUART := False;
  FSupportTPI := False;
  FBufferReady := False;
end;

destructor TUSBasp.Destroy;
begin
  UARTClose;
  Disconnect;
  FUSBaspDevices.Free;
  usbasp_finalization;
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
begin
  if FConnected and FSupportUART and not FUARTOpened then
  begin
    FLastUsbError := usbasp_uart_enable(FUSBaspDevices[FUSBaspID],
      ABaudRate, USBASP_UART_BYTES_8B or USBASP_UART_STOP_1BIT or
      USBASP_UART_PARITY_NONE);
    FUARTOpened := True;
    FThreadReceive := TUSBaspReceiveThread.Create(Self);
    FThreadSend := TUSBaspSendThread.Create(Self);
  end;
  Result := FUARTOpened;
end;

function TUSBasp.UARTClose: boolean;
begin
  if FConnected and FSupportUART and FUARTOpened then
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

initialization
  SendLock := TCriticalSection.Create;

finalization
  SendLock.Free;

end.
