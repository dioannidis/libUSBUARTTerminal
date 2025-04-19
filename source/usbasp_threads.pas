unit USBasp_Threads;

{

  This file is part of
    Nephelae USBasp HID UART.

  USB HID Read / Write threads.

  Copyright (C) 2022 - 2025 Dimitrios Chr. Ioannidis.
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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  USBasp_Definitions, SPSCRingBuffer;

type

  { TThreadHID_Base }

  TThreadHID_Base = class(TThread)
  protected
    FThreadEvent: TEvent;
    FBuffer: TSPSCRingBuffer;
    FUSBaspDevice: PUSBaspHIDIntf;
  public
    constructor Create(const AUSBaspDevice: PUSBaspHIDIntf;
      const ABuffer: TSPSCRingBuffer; const AThreadEvent: TEvent);
      reintroduce;
  end;


  { TThreadHID_Read }

  TThreadHID_Read = class(TThreadHID_Base)
  protected
    procedure Execute; override;
  end;

  { TThreadHID_Write }

  TThreadHID_Write = class(TThreadHID_Base)
  protected
    procedure Execute; override;
  end;

  { TThreadHID_UARTRead }

  TThreadHID_UARTRead = class(TThreadHID_Base)
    procedure Execute; override;
  end;

  { TThreadHID_UARTWrite }

  TThreadHID_UARTWrite = class(TThreadHID_Base)
  protected
    procedure Execute; override;
  end;

implementation

{ TThreadHID_Base }

constructor TThreadHID_Base.Create(const AUSBaspDevice: PUSBaspHIDIntf;
  const ABuffer: TSPSCRingBuffer; const AThreadEvent: TEvent);
begin
  inherited Create(False);
  FThreadEvent := AThreadEvent;
  FBuffer := ABuffer;
  FUSBaspDevice := AUSBaspDevice;
end;

{ TThreadHID_Read }

procedure TThreadHID_Read.Execute;
var
  USBAspHidPacket: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
  DataCount: byte;
begin
  repeat
    DataCount := FUSBaspDevice^.Device^.Read(USBAspHidPacket, FUSBaspDevice^.ReportSize);
    if DataCount > 0 then
    begin
      FBuffer.Write(USBAspHidPacket, DataCount);
      FThreadEvent.SetEvent;
    end;
  until Terminated;
end;

{ TThreadHID_Write }

procedure TThreadHID_Write.Execute;
var
  USBAspHidPacket: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  DataCount: byte = 0;
begin
  repeat
    if FBuffer.Empty then
      FThreadEvent.ResetEvent;
    FThreadEvent.WaitFor(INFINITE);

    FillChar(USBAspHidPacket, 8, 0);
    DataCount := FBuffer.Peek(USBAspHidPacket[1], 8);
    if DataCount > 0 then
    begin
      FUSBaspDevice^.Device^.Write(USBAspHidPacket,
        FUSBaspDevice^.ReportSize + 1);
      FBuffer.AdvanceReadIdx(DataCount);
    end;
  until Terminated;
end;

{ TThreadHID_UARTRead }

procedure TThreadHID_UARTRead.Execute;
var
  USBAspHidPacket: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
  SerialDataCount, DataCount: byte;
begin
  repeat
    DataCount := FUSBaspDevice^.Device^.Read(USBAspHidPacket, FUSBaspDevice^.ReportSize);
    if (DataCount > 0) and (USBAspHidPacket[7] > 0) then
    begin
      if (USBAspHidPacket[7] > 7) then
        SerialDataCount := 8
      else
        SerialDataCount := USBAspHidPacket[7];
      if FBuffer.Write(USBAspHidPacket, SerialDataCount) <> SerialDataCount then
        raise TExceptionClass.Create('Buffer OverRun ');
      FThreadEvent.SetEvent;
    end;
  until Terminated;
end;

{ TThreadHID_UARTWrite }

procedure TThreadHID_UARTWrite.Execute;
var
  USBAspHidPacket: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  SerialCountOrDataByte, DataCount: byte;
begin
  repeat
    if FBuffer.Empty then
      FThreadEvent.ResetEvent;
    FThreadEvent.WaitFor(INFINITE);

    DataCount := FBuffer.Peek(USBAspHidPacket[1], 8);
    SerialCountOrDataByte := DataCount;
    if (SerialCountOrDataByte = 8) and (USBAspHidPacket[8] = 7) then
      SerialCountOrDataByte := 7
    else
    if SerialCountOrDataByte < 8 then
      USBAspHidPacket[8] := SerialCountOrDataByte;

    if DataCount > 0 then
    begin
      SerialCountOrDataByte :=
        FUSBaspDevice^.Device^.Write(USBAspHidPacket, FUSBaspDevice^.ReportSize + 1);
      FBuffer.AdvanceReadIdx(DataCount);
    end;
  until Terminated;
end;

end.
