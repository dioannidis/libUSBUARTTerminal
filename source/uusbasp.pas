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

type
  TLineBreakMode = (lbmNoLineBreak, lbmCR, lbmLF, lbmCRLF);

  { TUSBasp }

  TUSBasp = class(TObject)
  private
    FConnected: boolean;
    FUSBaspDeviceSelectedID: byte;
    FUSBaspDevices: TUSBaspDeviceList;
    FBufferSend: array of byte;
    FSendLock: TCriticalSection;
    FLastUsbError: integer;
    FBaudRate: integer;
    FLineBreakMode: TLineBreakMode;
    FAutoScroll: boolean;
    FTimeStamp: boolean;
    FLastCharReceived: char;
    FMemo: TMemo;
    procedure SetAutoScroll(AValue: boolean);
    procedure SetConnected(AValue: boolean);
    procedure SetSelectedUSBaspDeviceID(AValue: byte);
    procedure SetTimeStamp(AValue: boolean);
    procedure SetSendBuffer(AValue: string);
  protected
    procedure SerialReceivedProcessing(const AMsg: string);
    procedure NoLineBreak(Data: PtrInt);
    procedure LineBreakCR(Data: PtrInt);
    procedure LineBreakLF(Data: PtrInt);
    procedure LineBreakCRLF(Data: PtrInt);
    procedure AutoScrollHack;
    function GetTimeStamp: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect: boolean;
    function Disconnect: boolean;
    procedure LoadUSBaspDevices;
    property USBaspID: byte read FUSBaspDeviceSelectedID
      write SetSelectedUSBaspDeviceID;
    property Connected: boolean read FConnected write SetConnected;
    property AutoScroll: boolean read FAutoScroll write SetAutoScroll;
    property TimeStamp: boolean read FTimeStamp write SetTimeStamp;
    property SendBuffer: string write SetSendBuffer;
    property USBaspDevices: TUSBaspDeviceList read FUSBaspDevices;
  end;

implementation

uses
  libusb;

type
  TRawSerialDataMsg = record
    AsString: string;
  end;
  PRawSerialDataMsg = ^TRawSerialDataMsg;

  { TUSBaspReceiveThread }

  TUSBaspReceiveThread = class(TThread)
  private
    FUSBasp: TUSBasp;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUSBasp: TUSBasp);
    destructor Destroy; override;
  end;

{ TUSBaspReceiveThread }

procedure TUSBaspReceiveThread.Execute;
const
  USBaspBufferSize = 254;
var
  USBaspBuffer: array of byte;
  RcvLen, LastUSBError: integer;
  RawSerialData: TBytes;
begin
  //LastUSBError := usbasp_uart_config(FUSBaspHandle, FBaudRate,
  //  USBASP_UART_PARITY_NONE or USBASP_UART_BYTES_8B or USBASP_UART_STOP_1BIT);

  //if LastUSBError <> 0 then
  //begin
  //  SerialReceivedProcessing(libusb_strerror(libusb_error(LastUSBError)));
  //  Sleep(20);
  //  Terminate;
  //  Exit;
  //end
  //else
  //  SetLength(USBaspBuffer, USBaspBufferSize);

  //while not terminated do
  //begin
  //  // Send
  //  // TODO:  needs a separate thread ...
  //  FSendLock.Acquire;
  //  try
  //    if Assigned(FBufferSend) then
  //    begin
  //      while RcvLen < Length(FBufferSend) do
  //      begin
  //        RcvLen := usbasp_uart_write(FUSBaspHandle, PChar(@FBufferSend[0]),
  //          Length(FBufferSend));
  //        if RcvLen < 0 then
  //        begin
  //          SerialReceivedProcessing(libusb_strerror(libusb_error(RcvLen)));
  //          Sleep(20);
  //          Terminate;
  //          Break;
  //        end;
  //      end;
  //      FBufferSend := nil;
  //    end;
  //  finally
  //    FSendLock.Release;
  //  end;

  //  // Receive
  //  RcvLen := usbasp_read(FUSBaspHandle, PChar(@USBaspBuffer[0]), USBaspBufferSize);
  //  if (RcvLen = 0) then
  //  begin
  //    Sleep(5);
  //    Continue;
  //  end;
  //  if RcvLen < 0 then
  //  begin
  //    SerialReceivedProcessing(libusb_strerror(libusb_error(RcvLen)));
  //    Sleep(20);
  //    Terminate;
  //    Break;
  //  end
  //  else
  //  begin
  //    SetLength(RawSerialData, RcvLen);
  //    Move(USBaspBuffer[0], RawSerialData[0], RcvLen);
  //    SerialReceivedProcessing(TEncoding.ASCII.GetString(RawSerialData));
  //    USBaspBuffer := nil;
  //    SetLength(USBaspBuffer, USBaspBufferSize);
  //  end;

  //end;

  //usbasp_uart_disable(FUSBaspHandle);
end;

constructor TUSBaspReceiveThread.Create(const AUSBasp: TUSBasp);
begin
  inherited Create(False);
  FUSBasp := AUSBasp;
end;

destructor TUSBaspReceiveThread.Destroy;
begin
  inherited Destroy;
end;

{ TUSBasp }

procedure TUSBasp.SetAutoScroll(AValue: boolean);
begin
  if FAutoScroll = AValue then
    Exit;
  FAutoScroll := AValue;
end;

procedure TUSBasp.SetConnected(AValue: boolean);
begin
  if FConnected = AValue then
    Exit;
  FConnected := AValue;
end;

procedure TUSBasp.SetSelectedUSBaspDeviceID(AValue: byte);
begin
  if FUSBaspDeviceSelectedID = AValue then
    Exit;
  if AValue > FUSBaspDevices.Count - 1 then
    Exit;
  FUSBaspDeviceSelectedID := AValue;
end;

procedure TUSBasp.SetTimeStamp(AValue: boolean);
begin
  if FTimeStamp = AValue then
    Exit;
  FTimeStamp := AValue;
end;

procedure TUSBasp.SetSendBuffer(AValue: string);
var
  tmp: TBytes;
begin
  FSendLock.Acquire;
  try
    tmp := BytesOf(AValue);
    SetLength(FBufferSend, Length(tmp));
    move(tmp[0], FBufferSend[0], Length(tmp));
  finally
    FSendLock.Release;
  end;
end;

procedure TUSBasp.LoadUSBaspDevices;
begin
  usbasp_devices(FUSBaspDevices);
  FUSBaspDevices.Count;
end;

procedure TUSBasp.SerialReceivedProcessing(const AMsg: string);
var
  RawSerialDataMsg: PRawSerialDataMsg;
begin
  New(RawSerialDataMsg);
  RawSerialDataMsg^.AsString := AMsg;
  case FLineBreakMode of
    lbmNoLineBreak: Application.QueueAsyncCall(@NoLineBreak, PtrInt(RawSerialDataMsg));
    lbmCR: Application.QueueAsyncCall(@LineBreakCR, PtrInt(RawSerialDataMsg));
    lbmLF: Application.QueueAsyncCall(@LineBreakLF, PtrInt(RawSerialDataMsg));
    lbmCRLF: Application.QueueAsyncCall(@LineBreakCRLF, PtrInt(RawSerialDataMsg))
  end;
end;

procedure TUSBasp.NoLineBreak(Data: PtrInt);
var
  RawSerialDataMsg: TRawSerialDataMsg;
  LastLine: integer;
begin
  RawSerialDataMsg := PRawSerialDataMsg(Data)^;
  try
    if (FMemo <> nil) and (not Application.Terminated) and
      (RawSerialDataMsg.AsString.Length > 0) then
    begin
      LastLine := FMemo.Lines.Count - 1;
      FMemo.Lines.BeginUpdate;
      FMemo.Lines[LastLine] :=
        FMemo.Lines[LastLine] + RawSerialDataMsg.AsString.Substring(0);
      AutoScrollHack;
      FMemo.Lines.EndUpdate;
    end;
  finally
    Dispose(PRawSerialDataMsg(Data));
  end;
end;

procedure TUSBasp.LineBreakCR(Data: PtrInt);
var
  RawSerialDataMsg: TRawSerialDataMsg;
  LastLine: integer;
begin
  RawSerialDataMsg := PRawSerialDataMsg(Data)^;
  try
    if (FMemo <> nil) and (not Application.Terminated) and
      (RawSerialDataMsg.AsString.Length > 0) then
    begin
      LastLine := FMemo.Lines.Count - 1;
      FMemo.Lines.BeginUpdate;
      FMemo.Lines[LastLine] :=
        FMemo.Lines[LastLine] + RawSerialDataMsg.AsString.Substring(0);
      AutoScrollHack;
      FMemo.Lines.EndUpdate;
    end;
  finally
    Dispose(PRawSerialDataMsg(Data));
  end;
end;

procedure TUSBasp.LineBreakLF(Data: PtrInt);
var
  RawSerialDataMsg: TRawSerialDataMsg;
  LastLine: integer;
begin
  RawSerialDataMsg := PRawSerialDataMsg(Data)^;
  try
    if (FMemo <> nil) and (not Application.Terminated) and
      (RawSerialDataMsg.AsString.Length > 0) then
    begin
      LastLine := FMemo.Lines.Count - 1;
      FMemo.Lines.BeginUpdate;
      FMemo.Lines[LastLine] :=
        FMemo.Lines[LastLine] + RawSerialDataMsg.AsString.Substring(0);
      AutoScrollHack;
      FMemo.Lines.EndUpdate;
    end;
  finally
    Dispose(PRawSerialDataMsg(Data));
  end;
end;

procedure TUSBasp.LineBreakCRLF(Data: PtrInt);
var
  RawSerialDataMsg: TRawSerialDataMsg;
  LastLine, i, j: integer;
begin
  RawSerialDataMsg := PRawSerialDataMsg(Data)^;
  try
    if (FMemo <> nil) and (not Application.Terminated) and
      (RawSerialDataMsg.AsString.Length > 0) then
    begin
      LastLine := FMemo.Lines.Count - 1;
      i := 1;
      j := i;
      if FLastCharReceived = #13 then
        RawSerialDataMsg.AsString := FLastCharReceived + RawSerialDataMsg.AsString;
      FMemo.Lines.BeginUpdate;
      while i < RawSerialDataMsg.AsString.Length do
      begin
        if (RawSerialDataMsg.AsString[i] = #13) and
          (RawSerialDataMsg.AsString[i + 1] = #10) then
        begin
          FMemo.Lines[LastLine] :=
            FMemo.Lines[LastLine] + RawSerialDataMsg.AsString.Substring(j - 1, i - j);
          if FTimeStamp then
            FMemo.Append(GetTimeStamp)
          else
            FMemo.Append('');
          LastLine := FMemo.Lines.Count - 1;
          Inc(i, 2);
          j := i;
        end
        else
          Inc(i);
      end;
      FMemo.Lines[LastLine] :=
        FMemo.Lines[LastLine] + RawSerialDataMsg.AsString.Substring(j - 1);
      AutoScrollHack;
      FMemo.Lines.EndUpdate;
      if RawSerialDataMsg.AsString[RawSerialDataMsg.AsString.Length] = #13 then
        FLastCharReceived := #13
      else
        FLastCharReceived := #0;
    end;
  finally
    Dispose(PRawSerialDataMsg(Data));
  end;
end;

procedure TUSBasp.AutoScrollHack;
begin
  if FAutoScroll then
  begin
    /////// HACK TO SCROLL BOTTOM //////////
    FMemo.SelStart := Length(FMemo.Lines.Text);
    FMemo.VertScrollBar.Position := 1000000;
    /////// HACK TO SCROLL BOTTOM //////////
  end;
end;

function TUSBasp.GetTimeStamp: string;
begin
  Result := TimeToStr(Now) + ': ';
end;

//constructor TUSBasp.Create(const ABaudRate: integer; AMemo: TMemo;
//  ALineBreakMode: TLineBreakMode; AAutoScroll: boolean; ATimeStamp: boolean);
//begin
//  FSendLock := TCriticalSection.Create;
//  //FBufferSend := nil;
//  //FTimeStamp := ATimeStamp;
//  //FAutoScroll := AAutoScroll;
//  //FBaudRate := ABaudRate;
//  //FLineBreakMode := ALineBreakMode;
//  FMemo := AMemo;
//end;

constructor TUSBasp.Create;
begin
  FUSBaspDevices := TUSBaspDeviceList.Create;
  FSendLock := TCriticalSection.Create;
  FUSBaspDeviceSelectedID := 255;
  FConnected := False;
end;

destructor TUSBasp.Destroy;
begin
  Disconnect;
  FSendLock.Free;
  FUSBaspDevices.Free;
  inherited Destroy;
end;

function TUSBasp.Connect: boolean;
begin
  if (not FConnected) and (FUSBaspDeviceSelectedID <> 255) then
  begin
    FLastUsbError := usbasp_open(FUSBaspDevices[FUSBaspDeviceSelectedID]);
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
    FLastUsbError := usbasp_close(FUSBaspDevices[FUSBaspDeviceSelectedID]);
    if FLastUsbError < 0 then
    begin

    end
    else
      FConnected := False;
  end;
  Result := FConnected;
end;

end.
