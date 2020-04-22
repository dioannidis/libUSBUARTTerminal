unit uUSBasp_Threads;

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
  Classes, SysUtils, syncobjs, uUSBasp;

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

  { TUSBaspSendThread }

  TUSBaspSendThread = class(TThread)
  private
    FUSBasp: TUSBasp;
    FWriteBufferLock: TCriticalSection;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFUSBasp: TUSBasp; const AWriteBufferLock: TCriticalSection);
    destructor Destroy; override;
  end;


implementation

uses
  libusb, usbasp_uart;

{ TUSBaspReceiveThread }

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
    RcvLen := usbasp_uart_read(FUSBasp.USBaspDevices[FUSBasp.USBaspID]^.Handle,
      PChar(@USBaspBuffer[0]), USBaspBufferSize);
    if (RcvLen = 0) then
    begin
      Sleep(1);
      Continue;
    end;
    if RcvLen < 0 then
    begin
      if Assigned(FUSBasp.OnUARTReceive) then
        FUSBasp.OnUARTReceive(libusb_strerror(libusb_error(RcvLen)));
      Sleep(20);
    end
    else
    begin
      SetLength(RawSerialData, RcvLen);
      Move(USBaspBuffer[0], RawSerialData[0], RcvLen);
      if Assigned(FUSBasp.OnUARTReceive) then
        FUSBasp.OnUARTReceive(TEncoding.ASCII.GetString(RawSerialData));
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
    FWriteBufferLock.Acquire;
    try
      if  FUSBasp.UARTWriteBuffer[0] = 1 then
      begin
        RcvLen := 0;
        while RcvLen < Length(FUSBasp.UARTWriteBuffer) - 1 do
        begin
          RcvLen := usbasp_uart_write(FUSBasp.USBaspDevices[FUSBasp.USBaspID]^.Handle,
            PChar(@FUSBasp.UARTWriteBuffer[1]), Length(FUSBasp.UARTWriteBuffer) - 1);
          if RcvLen < 0 then
          begin
            if Assigned(FUSBasp.OnUARTReceive) then
              FUSBasp.OnUARTReceive(libusb_strerror(libusb_error(RcvLen)));
            Sleep(20);
          end;
        end;
        FUSBasp.UARTWriteBuffer[0] := 0;
      end;
      Sleep(1);
    finally
      FWriteBufferLock.Release;
    end;
  end;
end;

constructor TUSBaspSendThread.Create(const AFUSBasp: TUSBasp;
  const AWriteBufferLock: TCriticalSection);
begin
  FUSBasp := AFUSBasp;
  FWriteBufferLock := AWriteBufferLock;
  inherited Create(False);
end;

destructor TUSBaspSendThread.Destroy;
begin
  inherited Destroy;
end;

end.
