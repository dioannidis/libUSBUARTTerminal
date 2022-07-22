unit USBasp_Threads;

{

  This file is part of
    Nephelae USBasp HID UART.

  USB HID Read / Write threads.

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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, USBasp_HIDAPI, USBasp_Definitions, SPSCRingBuffer;

type

  { TThreadHID_Read }

  TThreadHID_Read = class(TThread)
  private
    FBuffer: TSPSCRingBuffer;
    FUSBaspDevice: PUSBasp_HIDDevice;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUSBaspDevice: PUSBasp_HIDDevice;
      const ABuffer: TSPSCRingBuffer); reintroduce;
  end;

  { TThreadHID_UARTRead }

  TThreadHID_UARTRead = class(TThread)
  private
    FBuffer: TSPSCRingBuffer;
    FUSBaspDevice: PUSBasp_HIDDevice;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUSBaspDevice: PUSBasp_HIDDevice;
      const ABuffer: TSPSCRingBuffer); reintroduce;
  end;

  { TWriteRead }

  TThreadHID_UARTWrite = class(TThread)
  private
    FBuffer: TSPSCRingBuffer;
    FUSBaspDevice: PUSBasp_HIDDevice;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUSBaspDevice: PUSBasp_HIDDevice;
      const ABuffer: TSPSCRingBuffer); reintroduce;
  end;

implementation

{ TThreadHID_Read }

procedure TThreadHID_Read.Execute;
var
  USBAspHidPacket: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
  DataCount: byte;
begin
  repeat
    DataCount := usbasp_hidapi_read(FUSBaspDevice, USBAspHidPacket);
    if DataCount > 0 then
      FBuffer.Write(USBAspHidPacket, DataCount)
    else
      Sleep(2);
  until Terminated;
end;

constructor TThreadHID_Read.Create(const AUSBaspDevice: PUSBasp_HIDDevice;
  const ABuffer: TSPSCRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
  FUSBaspDevice := AUSBaspDevice;
end;

{ TThreadHID_UARTRead }

procedure TThreadHID_UARTRead.Execute;
var
  USBAspHidPacket: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
  SerialDataCount: byte;
begin
  repeat
    if usbasp_hidapi_read(FUSBaspDevice, USBAspHidPacket) > 0 then
    begin
      if (USBAspHidPacket[7] > 0) then
      begin
        if (USBAspHidPacket[7] > 7) then
          SerialDataCount := 8
        else
          SerialDataCount := USBAspHidPacket[7];
        if FBuffer.Write(USBAspHidPacket, SerialDataCount) <> SerialDataCount then
          raise TExceptionClass.Create('Buffer OverRun ');
      end;
    end
    else
      Sleep(2);
  until Terminated;
end;

constructor TThreadHID_UARTRead.Create(const AUSBaspDevice: PUSBasp_HIDDevice;
  const ABuffer: TSPSCRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
  FUSBaspDevice := AUSBaspDevice;
end;

{ TThreadHID_UARTWrite }

procedure TThreadHID_UARTWrite.Execute;
var
  USBAspHidPacket: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
  SerialCountOrDataByte: byte = 0;
begin
  repeat
    SerialCountOrDataByte := FBuffer.Peek(USBAspHidPacket, 8);

    if (SerialCountOrDataByte = 8) and (USBAspHidPacket[7] = 7) then
      SerialCountOrDataByte := 7
    else
      if SerialCountOrDataByte < 8 then
        USBAspHidPacket[7] := SerialCountOrDataByte;

    if SerialCountOrDataByte > 0 then
    begin
      if (usbasp_hidapi_write(FUSBaspDevice, USBAspHidPacket) = 8) then
        FBuffer.AdvanceReadIdx(SerialCountOrDataByte);
    end
    else
      Sleep(2);

  until Terminated;
end;

constructor TThreadHID_UARTWrite.Create(const AUSBaspDevice: PUSBasp_HIDDevice;
  const ABuffer: TSPSCRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
  FUSBaspDevice := AUSBaspDevice;
end;


end.
