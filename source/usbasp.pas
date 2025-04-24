unit USBasp;

{

  This file is part of Nephelae's Object Pascal FPUSBasp.

  USBasp Class.

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, hidapi,
  SPSCRingBuffer, USBasp_Definitions, USBasp_Threads;

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

  { TFPUSBasp }

  TFPUSBasp = class(TObject)
  private
    FMonitorReadEvent, FReceiveReadEvent, FTransmitWriteEvent: TEvent;
    FUSBaspID: byte;
    FUSBaspList: TUSBaspList;
    FUSBaspHIDIntfList: TUSBaspHIDIntfList;
    FUSBaspDeviceSelected: TUSBasp;
    FConnected: boolean;
    FUARTOpened: boolean;
    FLastUsbError: integer;
    FUARTReceiveBuffer, FUARTTransmitBuffer, FMonitorReadBuffer,
    FFeatureWriteBuffer: TSPSCRingBuffer;
    FUARTReceiveThread: TThreadHID_UARTRead;
    FUARTTransmitThread: TThreadHID_UARTWrite;
    FMonitorReadThread: TThreadHID_Read;
    FHidApiVersionString: string;
    procedure SetUSBaspID(const AValue: byte);
    function USBaspEnumerateHIDIntfs(var AUSBaspHIDDeviceList:
      TUSBaspHIDIntfList): integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AUSBaspDeviceID: byte = 0): boolean;
    function Disconnect: boolean;

    function UARTOpen(const ABaudRate, ADataBits, AParity, AStopBits: integer): boolean;
    function UARTClose: boolean;

    function EnumerateDevices: integer;
    function ChangeSerialNumber(const ASerialNumber: string): integer;

    property Connected: boolean read FConnected;
    property UARTOpened: boolean read FUARTOpened;

    property ReceiveBuffer: TSPSCRingBuffer read FUARTReceiveBuffer;
    property ReceiveEvent: TEvent read FReceiveReadEvent;

    property TransmitBuffer: TSPSCRingBuffer read FUARTTransmitBuffer;
    property TransmitEvent: TEvent read FTransmitWriteEvent;

    property MonitorBuffer: TSPSCRingBuffer read FMonitorReadBuffer;
    property MonitorEvent: TEvent read FMonitorReadEvent;

    property USBaspID: byte read FUSBaspID write SetUSBaspID;
    property USBaspDevice: TUSBasp read FUSBaspDeviceSelected;
    property USBaspDevices: TUSBaspList read FUSBaspList;

    property HidApiVersion: string read FHidApiVersionString;
  end;

implementation

uses
  StrUtils;

{ TFPUSBasp }

constructor TFPUSBasp.Create;
begin
  FUSBaspList := TUSBaspList.Create;
  FUSBaspHIDIntfList := TUSBaspHIDIntfList.Create;

  FUARTReceiveBuffer := TSPSCRingBuffer.Create(256);
  FUARTTransmitBuffer := TSPSCRingBuffer.Create(256);
  FMonitorReadBuffer := TSPSCRingBuffer.Create(256);

  FUSBaspID := USBaspIDNotFound;
  FConnected := False;
  FUARTOpened := False;

  FMonitorReadEvent := TEvent.Create(nil, False, False, 'MONRD_' + IntToStr(Random(999)));
  FReceiveReadEvent := TEvent.Create(nil, False, False, 'RCDRD_' + IntToStr(Random(999)));
  FTransmitWriteEvent := TEvent.Create(nil, True, False, 'TRNWR_' + IntToStr(Random(999)));

  FHidApiVersionString := HidApiVersionStr;
end;

destructor TFPUSBasp.Destroy;
begin
  UARTClose();
  Disconnect;

  FreeAndNil(FUARTReceiveBuffer);
  FreeAndNil(FUARTTransmitBuffer);
  FreeAndNil(FMonitorReadBuffer);

  FUSBaspHIDIntfList.Free;
  FUSBaspList.Free;

  FMonitorReadEvent.Free;
  FReceiveReadEvent.Free;
  FTransmitWriteEvent.Free;

  inherited Destroy;
end;

function TFPUSBasp.Connect(const AUSBaspDeviceID: byte = 0): boolean;
begin
  if (not FConnected) and (FUSBaspList.Count > 0) and
    (AUSBaspDeviceID in [0..(FUSBaspList.Count - 1)]) then
  begin
    FUSBaspID := AUSBaspDeviceID;
    FUSBaspDeviceSelected := FUSBaspList[FUSBaspID]^;
    if FUSBaspList[FUSBaspID]^.HasMonitorIntf then
    begin
      FUSBaspList[FUSBaspID]^.MonitorInterface^.Device :=
        THidDevice.OpenPath(FUSBaspList[FUSBaspID]^.MonitorInterface^.Path);

      FUSBaspList[FUSBaspID]^.MonitorInterface^.Device^.SetNonBlocking(0);

      FMonitorReadThread := TThreadHID_Read.Create(
        FUSBaspList[FUSBaspID]^.MonitorInterface, FMonitorReadBuffer,
        FMonitorReadEvent);
    end;
    FConnected := True;
  end;
  Result := FConnected;
end;

function TFPUSBasp.Disconnect: boolean;
begin
  if FConnected then
  begin
    if FUARTOpened then
      UARTClose;

    if FUSBaspList[FUSBaspID]^.HasMonitorIntf then
    begin
      FMonitorReadThread.Terminate;
      FMonitorReadThread.WaitFor;
      FreeAndNil(FMonitorReadThread);
      FUSBaspList[FUSBaspID]^.MonitorInterface^.Device^.Close;
    end;

    FConnected := False;
  end;
  Result := FConnected;
end;

function TFPUSBasp.UARTOpen(const ABaudRate, ADataBits, AParity,
  AStopBits: integer): boolean;
var
  Buffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  Prescaler: word;
begin
  if FConnected and not FUARTOpened then
  begin
    FUSBaspList[FUSBaspID]^.UARTInterface^.Device :=
      THidDevice.OpenPath(FUSBaspList[FUSBaspID]^.UARTInterface^.Path);

    FUSBaspList[FUSBaspID]^.UARTInterface^.Device^.SetNonBlocking(0);

    Prescaler := FUSBaspList[FUSBaspID]^.CrystalOsc div 8 div ABaudRate - 1;

    Buffer[0] := 0;
    Buffer[1] := lo(Prescaler);
    Buffer[2] := hi(Prescaler);
    Buffer[3] := ADataBits or AStopBits or AParity;

    FUSBaspList[FUSBaspID]^.UARTInterface^.Device^.SendFeatureReport(Buffer,
      FUSBaspList[FUSBaspID]^.UARTInterface^.ReportSize + 1);

    FUARTReceiveThread := TThreadHID_UARTRead.Create(
      FUSBaspList[FUSBaspID]^.UARTInterface, FUARTReceiveBuffer, FReceiveReadEvent);
    FUARTTransmitThread := TThreadHID_UARTWrite.Create(
      FUSBaspList[FUSBaspID]^.UARTInterface, FUARTTransmitBuffer, FTransmitWriteEvent);

    FUARTOpened := True;
  end;

  Result := FUARTOpened;
end;

function TFPUSBasp.UARTClose: boolean;
var
  Buffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
begin
  if FConnected and FUARTOpened then
  begin

    Buffer[0] := 0;
    Buffer[1] := 0;
    Buffer[2] := 0;
    Buffer[3] := 0;

    FUSBaspList[FUSBaspID]^.UARTInterface^.Device^.SendFeatureReport(Buffer,
      FUSBaspList[FUSBaspID]^.UARTInterface^.ReportSize + 1);

    FUARTReceiveThread.Terminate;
    FUARTReceiveThread.WaitFor;
    FreeAndNil(FUARTReceiveThread);

    FUARTTransmitThread.Terminate;
    FTransmitWriteEvent.SetEvent;
    FUARTTransmitThread.WaitFor;
    FreeAndNil(FUARTTransmitThread);

    FUSBaspList[FUSBaspID]^.UARTInterface^.Device^.Close;

    FUARTOpened := False;
  end;

  Result := FUARTOpened;
end;

procedure TFPUSBasp.SetUSBaspID(const AValue: byte);
begin
  if FUSBaspID = AValue then
    Exit;
  if FConnected or (FUSBaspList.Count = 0) then
    Exit;
  if AValue in [0..(FUSBaspList.Count - 1)] then
  begin
    FUSBaspID := AValue;
    FUSBaspDeviceSelected := FUSBaspList[FUSBaspID]^;
  end;
end;

function TFPUSBasp.USBaspEnumerateHIDIntfs(
  var AUSBaspHIDDeviceList: TUSBaspHIDIntfList): integer;
var
  HidEnumerateList, HidItem: PHidDeviceInfo;
  USBaspHIDDevice: PUSBaspHIDIntf = nil;
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

function TFPUSBasp.EnumerateDevices: integer;
var
  USBasp: PUSBasp;
  USBaspHIDIntf: PUSBaspHIDIntf;
  USBaspFound: boolean;
  FeatureReportBuffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  tmpIntf: PUSBaspHIDIntf;
begin
  Result := 0;
  if not FConnected then
  begin
    FUSBaspList.FreeItems;
    FUSBaspList.Clear;
    if USBaspEnumerateHIDIntfs(FUSBaspHIDIntfList) > 0 then
    begin
      for USBaspHIDIntf in FUSBaspHIDIntfList do
      begin
        USBaspFound := False;
        for USBasp in FUSBaspList do
{$ifdef MSWINDOWS}
          if IsEqualGUID(USBasp^.ContainerID, USBaspHIDIntf^.ContainerID) then
{$endif}
{$ifdef UNIX}
  {$ifdef LINUX}
          if USBasp^.Path = USBaspHIDIntf^.Path.Remove(USBaspHIDIntf^.Path.LastIndexOf(':')) then
  {$endif}
  {$ifdef BSD}
    {$ifdef FREEBSD}
          if USBasp^.Path = USBaspHIDIntf^.Path.Remove(USBaspHIDIntf^.Path.LastIndexOf('.')) then
    {$endif}
    {$ifdef DARWIN}
    {$endif}
  {$endif}
{$endif}
          begin
            USBaspFound := True;
            Break;
          end;

        if not USBaspFound then
        begin
          New(tmpIntf);
          try
            tmpIntf^.Device := USBaspHIDIntf^.Device^.OpenPath(USBaspHIDIntf^.Path);
            tmpIntf^.Device^.GetFeatureReport(FeatureReportBuffer, USBaspHIDIntf^.ReportSize + 1);
            tmpIntf^.Device^.Close;
          finally
            Dispose(tmpIntf);
          end;

          New(USBasp);
{$ifdef MSWINDOWS}
          USBasp^.ContainerID := USBaspHIDIntf^.ContainerID;
{$endif}
{$ifdef UNIX}
  {$ifdef LINUX}
          USBasp^.Path := USBaspHIDIntf^.Path.Remove(USBaspHIDIntf^.Path.LastIndexOf(':'));
  {$endif}
  {$ifdef BSD}
    {$ifdef FREEBSD}
          USBasp^.Path := USBaspHIDIntf^.Path.Remove(USBaspHIDIntf^.Path.LastIndexOf('.'));
    {$endif}
    {$ifdef DARWIN}
    {$endif}
  {$endif}
{$endif}
          USBasp^.Manufacturer := USBaspHIDIntf^.Manufacturer;
          USBasp^.ProductName := USBaspHIDIntf^.Product;
          USBasp^.SerialNumber := USBaspHIDIntf^.Serial;
          USBasp^.HasMonitorIntf := False;
          USBasp^.HasUart := False;

          case FeatureReportBuffer[6] of
            USBASP_CAP_12MHZ_CLOCK: USBasp^.CrystalOsc := 12000000;
            USBASP_CAP_16MHZ_CLOCK: USBasp^.CrystalOsc := 16000000;
            USBASP_CAP_18MHZ_CLOCK: USBasp^.CrystalOsc := 18000000;
            USBASP_CAP_20MHZ_CLOCK: USBasp^.CrystalOsc := 20000000;
          end;

          USBasp^.HasHIDUart :=
            (FeatureReportBuffer[5] and USBASP_CAP_7_HID_UART) = USBASP_CAP_7_HID_UART;
          USBasp^.HasPDI := (FeatureReportBuffer[5] and USBASP_CAP_PDI) = USBASP_CAP_PDI;
          USBasp^.HasTPI := (FeatureReportBuffer[5] and USBASP_CAP_0_TPI) = USBASP_CAP_0_TPI;
          USBasp^.HasSNWrite :=
            (FeatureReportBuffer[5] and USBASP_CAP_2_SNHIDUPDATE) = USBASP_CAP_2_SNHIDUPDATE;

          FUSBaspList.Add(USBasp);
        end;

        case USBaspHIDIntf^.InterfaceNumber of
          1:
          begin
            USBasp^.UARTInterface := USBaspHIDIntf;
            USBasp^.FirmwareVersion := USBaspHIDIntf^.FirmwareVersion;
          end;
          2:
          begin
            USBasp^.MonitorInterface := USBaspHIDIntf;
            USBasp^.HasMonitorIntf := True;
          end;
        end;

      end;
    end;
  end;
  Result := FUSBaspList.Count;
  if Result = 0 then
    FUSBaspID := USBaspIDNotFound;
end;

function TFPUSBasp.ChangeSerialNumber(const ASerialNumber: string): integer;
var
  Buffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  HidSize: SizeInt;
  ErrorCode: integer;
  SerNumValue: Word;
begin
  Result := 0;
  if FConnected and not FUARTOpened then
  begin
    Val(ASerialNumber, SerNumValue, ErrorCode);

    // Add Report ID
    Buffer[0] := $00;

    Buffer[1] := lo(SerNumValue);
    Buffer[2] := Hi(SerNumValue);
    Buffer[4] := $01;

    FUSBaspList[FUSBaspID]^.MonitorInterface^.Device^.SendFeatureReport(Buffer,
      FUSBaspList[FUSBaspID]^.MonitorInterface^.ReportSize + 1);

  end;
end;

initialization
  HidInit();

finalization;
  HidExit();

end.
