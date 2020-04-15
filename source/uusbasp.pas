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

  { TUSBaspThread }

  TUSBaspThread = class(TThread)
  private
    FUSBaspHandle: USBaspUART;
    FBufferSend: array of byte;
    FSendLock: TCriticalSection;
    FBaudRate: integer;
    FLineBreakMode: TLineBreakMode;
    FAutoScroll: boolean;
    FTimeStamp: boolean;
    FLastCharReceived: char;
    FMemo: TMemo;
    procedure SetAutoScroll(AValue: boolean);
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
    procedure Execute; override;
  public
    constructor Create(const ABaudRate: integer; AMemo: TMemo;
      ALineBreakMode: TLineBreakMode; AAutoScroll: boolean; ATimeStamp: boolean);
    destructor Destroy; override;
    property AutoScroll: boolean read FAutoScroll write SetAutoScroll;
    property TimeStamp: boolean read FTimeStamp write SetTimeStamp;
    property SendBuffer: string write SetSendBuffer;
  end;

implementation

type
  TRawSerialDataMsg = record
    AsString: string;
  end;
  PRawSerialDataMsg = ^TRawSerialDataMsg;

{ TUSBaspThread }

procedure TUSBaspThread.SetAutoScroll(AValue: boolean);
begin
  if FAutoScroll = AValue then
    Exit;
  FAutoScroll := AValue;
end;

procedure TUSBaspThread.SetTimeStamp(AValue: boolean);
begin
  if FTimeStamp = AValue then
    Exit;
  FTimeStamp := AValue;
end;

procedure TUSBaspThread.SetSendBuffer(AValue: string);
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

procedure TUSBaspThread.SerialReceivedProcessing(const AMsg: string);
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

procedure TUSBaspThread.NoLineBreak(Data: PtrInt);
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

procedure TUSBaspThread.LineBreakCR(Data: PtrInt);
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

procedure TUSBaspThread.LineBreakLF(Data: PtrInt);
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

procedure TUSBaspThread.LineBreakCRLF(Data: PtrInt);
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

procedure TUSBaspThread.AutoScrollHack;
begin
  if FAutoScroll then
  begin
    /////// HACK TO SCROLL BOTTOM //////////
    FMemo.SelStart := Length(FMemo.Lines.Text);
    FMemo.VertScrollBar.Position := 1000000;
    /////// HACK TO SCROLL BOTTOM //////////
  end;
end;

function TUSBaspThread.GetTimeStamp: string;
begin
  Result := TimeToStr(Now) + ': ';
end;

procedure TUSBaspThread.Execute;
const
  USBaspBufferSize = 254;
var
  USBaspBuffer: array of byte;
  RcvLen: integer;
  RawSerialData: TBytes;
begin
  usbasp_uart_config(FUSBaspHandle, FBaudRate,
    USBASP_UART_PARITY_NONE or USBASP_UART_BYTES_8B or USBASP_UART_STOP_1BIT);

  SetLength(USBaspBuffer, USBaspBufferSize);
  while not terminated do
  begin
    // Send
    // TODO:  needs a separate thread ...
    FSendLock.Acquire;
    try
      if Assigned(FBufferSend) then
      begin
        while RcvLen < Length(FBufferSend) do
          RcvLen := usbasp_uart_write(FUSBaspHandle, PChar(@FBufferSend[0]),
            Length(FBufferSend));
        FBufferSend := nil;
      end;
    finally
      FSendLock.Release;
    end;

    // Receive
    RcvLen := usbasp_uart_read(FUSBaspHandle, PChar(@USBaspBuffer[0]), USBaspBufferSize);
    if (RcvLen = 0) then
    begin
      Sleep(5);
      Continue;
    end;
    if RcvLen < 0 then
      Write('-USBERROR-')
    else
    begin
      SetLength(RawSerialData, RcvLen);
      Move(USBaspBuffer[0], RawSerialData[0], RcvLen);
      SerialReceivedProcessing(TEncoding.ASCII.GetString(RawSerialData));
      USBaspBuffer := nil;
      SetLength(USBaspBuffer, USBaspBufferSize);
    end;

  end;

  usbasp_uart_disable(FUSBaspHandle);
end;

constructor TUSBaspThread.Create(const ABaudRate: integer; AMemo: TMemo;
  ALineBreakMode: TLineBreakMode; AAutoScroll: boolean; ATimeStamp: boolean);
begin
  inherited Create(False);
  FSendLock := TCriticalSection.Create;
  FBufferSend := nil;
  FTimeStamp := ATimeStamp;
  FAutoScroll := AAutoScroll;
  FBaudRate := ABaudRate;
  FLineBreakMode := ALineBreakMode;
  FMemo := AMemo;
end;

destructor TUSBaspThread.Destroy;
begin
  FSendLock.Free;
  inherited Destroy;
end;

end.
