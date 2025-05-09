unit FPUSBaspGUIUARTTerminal;

{

  This file is part of Nephelae's Object Pascal FPUSBasp.

  LIBUSB/HIDAPI USBasp UART Terminal GUI.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, ComboEx, ExtCtrls, MaskEdit, XMLPropStorage, Buttons,
  DateUtils, syncobjs, uSplashAbout, uversion,
  USBasp, USBasp_Definitions, SPSCRingBuffer;

type

  TRawSerialDataMsg = record
    AsString: string;
    BreakChar: char;
  end;
  PRawSerialDataMsg = ^TRawSerialDataMsg;

  TRawMonitorDataMsg = record
    ProgrammerState: string;
    UARTState: string;
  end;
  PRawMonitorDataMsg = ^TRawMonitorDataMsg;

  { TThreadMonitor }

  TThreadMonitor = class(TThread)
  private
    FMonitorEvent: TEvent;
    FBuffer: TSPSCRingBuffer;
    FData: TByteArray;
    FDataCount: integer;
    procedure DoMonitor;
  protected
    procedure Execute; override;
  public
    constructor Create(const ABuffer: TSPSCRingBuffer; const AMonitorEvent: TEvent);
      reintroduce;
  end;

  { TThreadUARTRead }

  TThreadUARTRead = class(TThread)
  private
    FReceiveEvent: TEvent;
    FBuffer: TSPSCRingBuffer;
    FSerialData: TByteArray;
    FSerialDataCount: integer;
    procedure DoUARTReceiveProcessing;
  protected
    procedure Execute; override;
  public
    constructor Create(const ABuffer: TSPSCRingBuffer; const AReceiveEvent: TEvent);
      reintroduce;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    btnClearMemo: TButton;
    btnClose: TButton;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnOpen: TButton;
    btnSend: TButton;
    cbxBaudRate: TComboBoxEx;
    cbxParity: TComboBoxEx;
    cbxStopBits: TComboBoxEx;
    cbxLineBreak: TComboBoxEx;
    cbxUSBaspDevice: TComboBox;
    cbxWordWrap: TCheckBox;
    ckbAutoScroll: TCheckBox;
    ckbTimeStamp: TCheckBox;
    cbxDataBits: TComboBoxEx;
    edtSend: TEdit;
    gbNoRuntimeSettings: TGroupBox;
    gbRuntimeSettings: TGroupBox;
    gbUART: TGroupBox;
    gbInfo: TGroupBox;
    lblChangeSerial: TLabel;
    lblBaud: TLabel;
    lblFWVersion: TLabel;
    lblHasPDI: TLabel;
    lblHasSNWrite: TLabel;
    lblHasTPI: TLabel;
    lblManufacturer: TLabel;
    lblOSC: TLabel;
    lblStopBits: TLabel;
    lblParity: TLabel;
    lblDataBits: TLabel;
    lblLineBreak: TLabel;
    lblMemoBufferLines: TLabel;
    lblUSBaspDevice: TLabel;
    AppMainMenu: TMainMenu;
    medtSerialNumber: TMaskEdit;
    medtMemoBufferLines: TMaskEdit;
    miAbout: TMenuItem;
    mmDisplay: TMemo;
    miFile: TMenuItem;
    miExit: TMenuItem;
    sbtnRefresh: TSpeedButton;
    AppStatusBar: TStatusBar;
    AppXMLPropStorage: TXMLPropStorage;
    sbtnChangeSerial: TSpeedButton;
    SplashAbout: TSplashAbout;
    procedure AppStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure btnClearMemoClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure cbxUSBaspDeviceChange(Sender: TObject);
    procedure cbxWordWrapChange(Sender: TObject);
    procedure edtSendKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure medtMemoBufferLinesChange(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure mmDisplayChange(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure sbtnChangeSerialClick(Sender: TObject);
    procedure sbtnRefreshClick(Sender: TObject);
  private
    FUSBasp: TFPUSBasp;
    FLastCharReceived: char;
    FUARTWantedState: boolean;

    FUARTLastStateChange: TDateTime;

    FThreadUARTRead: TThreadUARTRead;

    FThreadMonitor: TThreadMonitor;

    procedure USBaspMonitor(Data: PtrInt);

    procedure ToggleGUI;

    procedure NoLineBreak(Data: PtrInt);
    procedure LineBreakCRorLF(Data: PtrInt);
    procedure LineBreakCRLF(Data: PtrInt);

    procedure AutoScrollHack;
    function GetTimestamp: string;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TThreadMonitor }

procedure TThreadMonitor.DoMonitor;
var
  RawMonitorDataMsg: PRawMonitorDataMsg;
begin
  if (frmMain.FUSBasp.Connected) and (not Application.Terminated) then
  begin
    New(RawMonitorDataMsg);
    case (FData[7] and 7) of
      PROG_STATE_IDLE: RawMonitorDataMsg^.ProgrammerState := 'IDLE';
      PROG_STATE_WRITEFLASH: RawMonitorDataMsg^.ProgrammerState := 'WRITE FLASH';
      PROG_STATE_READFLASH: RawMonitorDataMsg^.ProgrammerState := 'READ FLASH';
      PROG_STATE_READEEPROM: RawMonitorDataMsg^.ProgrammerState := 'READ EEPROM';
      PROG_STATE_WRITEEEPROM: RawMonitorDataMsg^.ProgrammerState := 'WRIT EEEPROM';
      PROG_STATE_TPI_READ: RawMonitorDataMsg^.ProgrammerState := 'TPI READ';
      PROG_STATE_TPI_WRITE: RawMonitorDataMsg^.ProgrammerState := 'TPI WRITE';
      PROG_STATE_SET_REPORT: RawMonitorDataMsg^.ProgrammerState := 'SET REPORT';
      else
        RawMonitorDataMsg^.ProgrammerState := 'UNKNOWN';
    end;
    if (FData[7] and UART_STATE_ENABLED) = UART_STATE_ENABLED then
      RawMonitorDataMsg^.UARTState := 'Enabled'
    else
      RawMonitorDataMsg^.UARTState := 'Disabled';
    Application.QueueAsyncCall(@frmMain.USBaspMonitor, PtrInt(RawMonitorDataMsg));
  end;
end;

procedure TThreadMonitor.Execute;
begin
  repeat
    FMonitorEvent.WaitFor(INFINITE);
    FDataCount := FBuffer.Read(FData, FBuffer.Size);
    if (FDataCount > 0) and (not Application.Terminated) then
      Synchronize(@DoMonitor)
  until Terminated;
end;

constructor TThreadMonitor.Create(const ABuffer: TSPSCRingBuffer;
  const AMonitorEvent: TEvent);
begin
  inherited Create(False);
  FMonitorEvent := AMonitorEvent;
  FBuffer := ABuffer;
end;

{ TThreadUARTRead }

procedure TThreadUARTRead.DoUARTReceiveProcessing;
var
  RawSerialDataMsg: PRawSerialDataMsg;
begin
  if (frmMain.FUSBasp.UARTOpened) and (not Application.Terminated) then
  begin
    New(RawSerialDataMsg);
    RawSerialDataMsg^.AsString :=
      TEncoding.ASCII.GetString(FSerialData, 0, FSerialDataCount);
    case frmMain.cbxLineBreak.ItemIndex of
      0: Application.QueueAsyncCall(@frmMain.NoLineBreak, PtrInt(RawSerialDataMsg));
      1:
      begin
        RawSerialDataMsg^.BreakChar := #13;
        Application.QueueAsyncCall(@frmMain.LineBreakCRorLF, PtrInt(RawSerialDataMsg));
      end;
      2:
      begin
        RawSerialDataMsg^.BreakChar := #10;
        Application.QueueAsyncCall(@frmMain.LineBreakCRorLF, PtrInt(RawSerialDataMsg));
      end;
      3: Application.QueueAsyncCall(@frmMain.LineBreakCRLF, PtrInt(RawSerialDataMsg))
    end;
  end;
end;

procedure TThreadUARTRead.Execute;
begin
  repeat
    FReceiveEvent.WaitFor(INFINITE);
    FSerialDataCount := FBuffer.Read(FSerialData, FBuffer.Size);
    if (FSerialDataCount > 0) and (not Application.Terminated) then
      Synchronize(@DoUARTReceiveProcessing)
  until Terminated;
end;

constructor TThreadUARTRead.Create(const ABuffer: TSPSCRingBuffer;
  const AReceiveEvent: TEvent);
begin
  inherited Create(False);
  FReceiveEvent := AReceiveEvent;
  FBuffer := ABuffer;
end;

{ TfrmMain }

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  FUSBasp.Connect(cbxUSBaspDevice.ItemIndex);
  medtSerialNumber.Text := FUSBasp.USBaspDevice.SerialNumber;
  ToggleGUI;
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  medtSerialNumber.Text := '0000';
  FUSBasp.Disconnect;
  ToggleGUI;
end;

procedure TfrmMain.cbxUSBaspDeviceChange(Sender: TObject);
begin
  if FUSBasp.USBaspDevices.Count > 0 then
  begin
    FUSBasp.USBaspID := cbxUSBaspDevice.ItemIndex;
    lblManufacturer.Caption := 'Manufacturer: ' + FUSBasp.USBaspDevice.Manufacturer;
    lblFWVersion.Caption := 'FW: ' +
      FUSBasp.USBaspDevice.FirmwareVersion;
    lblOSC.Caption := 'OSC: ' + FUSBasp.USBaspDevice.CrystalOsc.ToString() + ' Hz';
    lblHasTPI.Caption := 'TPI: ' + BoolToStr(FUSBasp.USBaspDevice.HasTPI, 'On', 'Off');
    lblHasPDI.Caption := 'PDI: ' + BoolToStr(FUSBasp.USBaspDevice.HasPDI, 'On', 'Off');
    lblHasSNWrite.Caption := 'SNW: ' + BoolToStr(FUSBasp.USBaspDevice.HasSNWrite,
      'On', 'Off');
  end;
  ToggleGUI;
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  FUSBasp.UARTOpen(TUARTBaudRate[cbxBaudRate.ItemIndex],
    TUARTDataBits[cbxDataBits.ItemIndex],
    TUARTParity[cbxParity.ItemIndex], TUARTStopBit[cbxStopBits.ItemIndex]);
  ToggleGUI;
  FUARTWantedState := True;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  FUARTWantedState := False;
  FUSBasp.UARTClose;
  ToggleGUI;
end;

procedure TfrmMain.cbxWordWrapChange(Sender: TObject);
begin
  mmDisplay.WordWrap := cbxWordWrap.State = cbChecked;
  mmDisplay.Clear;
end;

procedure TfrmMain.edtSendKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    btnSendClick(Self);
end;

procedure TfrmMain.btnClearMemoClick(Sender: TObject);
begin
  mmDisplay.Clear;
end;

procedure TfrmMain.AppStatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  w, h, x, y: integer;
begin
  StatusBar.Canvas.Font.Color := clWhite;
  case Panel.Text of
    'Enabled': StatusBar.Canvas.Brush.Color := clGreen;
    'IDLE': StatusBar.Canvas.Brush.Color := clGreen;
    'Disabled': StatusBar.Canvas.Brush.Color := clRed;
    'UNKNOWN': StatusBar.Canvas.Brush.Color := clRed;
    else
    begin
      StatusBar.Canvas.Font.Color := clBlack;
      StatusBar.Canvas.Brush.Color := clLime;
    end;
  end;
  StatusBar.Canvas.FillRect(Rect);
  w := StatusBar.Canvas.TextWidth(Panel.Text);
  case Panel.Alignment of
    taLeftJustify: x := Rect.Left + 2;
    taRightJustify: x := Rect.Right - 2 - w;
    taCenter: x := Rect.Left + (Rect.Width - w) div 2;
  end;
  h := StatusBar.Canvas.TextHeight(Panel.Text);
  y := Rect.Top + (Rect.Height - h) div 2;
  StatusBar.Canvas.TextOut(x, y, Panel.Text);
end;

procedure TfrmMain.btnSendClick(Sender: TObject);
begin
  case cbxLineBreak.ItemIndex of
    1: edtSend.Text := edtSend.Text + #13;
    2: edtSend.Text := edtSend.Text + #10;
    3: edtSend.Text := edtSend.Text + #13#10;
  end;
  FUSBasp.TransmitBuffer.Write(TEncoding.ASCII.GetBytes(edtSend.Text)[0],
    Length(edtSend.Text));
  FUSBasp.TransmitEvent.SetEvent;
  edtSend.Text := '';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  FLastCharReceived := #0;

  FUSBasp := TFPUSBasp.Create();
  FThreadMonitor := TThreadMonitor.Create(FUSBasp.MonitorBuffer, FUSBasp.MonitorEvent);
  FThreadUARTRead := TThreadUARTRead.Create(FUSBasp.ReceiveBuffer, FUSBasp.ReceiveEvent);

  if FileExists(ChangeFileExt(Application.ExeName, '.xml')) then
  begin
    Position := poDesigned;
    DefaultMonitor := dmActiveForm;
  end;

  SplashAbout := TSplashAbout.Create(Self);
  SplashAbout.DelaySeconds := 5;
  SplashAbout.Author := 'Dimitrios Chr. Ioannidis';
  SplashAbout.LicenseFile := saMIT;
  SplashAbout.UserTitle := 'USBasp HID UART Terminal';
  SplashAbout.Description :=
    'For USBasp improved firmware v1.11 and up.'#13#10'https://github.com/dioannidis/usbasp';
  SplashAbout.ShowDescription := True;
  SplashAbout.BackGroundColor := clDefault;

  SplashAbout.ShowSplash;

  AppStatusBar.Panels[0].Text := 'v' + GetFileVersion;
  AppStatusBar.Panels[1].Text := 'HIDAPI v' + FUSBasp.HidApiVersion;

  ToggleGUI;

  FUARTWantedState := False;
  FUARTLastStateChange := Now;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin

  FUSBasp.ReceiveEvent.SetEvent;
  FThreadUARTRead.Terminate;
  FThreadUARTRead.WaitFor;
  FreeAndNil(FThreadUARTRead);

  FUSBasp.MonitorEvent.SetEvent;
  FThreadMonitor.Terminate;
  FThreadMonitor.WaitFor;
  FreeAndNil(FThreadMonitor);

  FUSBasp.Free;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    btnDisconnectClick(Self);
end;

procedure TfrmMain.medtMemoBufferLinesChange(Sender: TObject);
begin
  if StrToInt(medtMemoBufferLines.Text) < 100 then
    medtMemoBufferLines.Text := '100';
end;

procedure TfrmMain.miAboutClick(Sender: TObject);
begin
  SplashAbout.ShowDescription := False;
  SplashAbout.ShowAbout;
end;

procedure TfrmMain.mmDisplayChange(Sender: TObject);
begin
  if mmDisplay.Lines.Count > StrToInt(medtMemoBufferLines.Text) + 10 then
  begin
    mmDisplay.Lines.BeginUpdate;
    while mmDisplay.Lines.Count > StrToInt(medtMemoBufferLines.Text) do
      mmDisplay.Lines.Delete(0);
    mmDisplay.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.sbtnChangeSerialClick(Sender: TObject);
begin
  if medtSerialNumber.Text <> FUSBasp.USBaspDevice.SerialNumber then
  begin
    FUSBasp.ChangeSerialNumber(medtSerialNumber.Text);
    btnDisconnectClick(Self);
    sbtnRefreshClick(Self);
  end;
end;

procedure TfrmMain.sbtnRefreshClick(Sender: TObject);
var
  i: byte;
begin
  if FUSBasp.Connected then
    Exit;
  cbxUSBaspDevice.Items.BeginUpdate;
  cbxUSBaspDevice.Items.Clear;
  if FUSBasp.EnumerateDevices > 0 then
  begin
    for i := 0 to FUSBasp.USBaspDevices.Count - 1 do
      cbxUSBaspDevice.AddItem(FUSBasp.USBaspDevices[i]^.ProductName +
        ' [' + FUSBasp.USBaspDevices[i]^.SerialNumber + ']', nil);
    cbxUSBaspDevice.ItemIndex := 0;
    cbxUSBaspDeviceChange(self);
  end
  else
  begin
    cbxUSBaspDevice.AddItem('No USBasp Device Found', nil);
    cbxUSBaspDevice.ItemIndex := 0;
  end;
  cbxUSBaspDevice.Items.EndUpdate;
end;

procedure TfrmMain.USBaspMonitor(Data: PtrInt);
var
  RawMonitorDataMsg: TRawMonitorDataMsg;
begin
  RawMonitorDataMsg := PRawMonitorDataMsg(Data)^;
  try
    if FUSBasp.Connected then
    begin
      AppStatusBar.Panels[3].Text := RawMonitorDataMsg.UARTState;
      AppStatusBar.Panels[5].Text := RawMonitorDataMsg.ProgrammerState;
    end
    else
    begin
      AppStatusBar.Panels[3].Text := 'Disabled';
      AppStatusBar.Panels[5].Text := 'UNKNOWN';
    end;
  finally
    Dispose(PRawMonitorDataMsg(Data));
  end;
end;

procedure TfrmMain.ToggleGUI;
begin
  medtSerialNumber.Enabled := FUSBasp.Connected and not FUSBasp.UARTOpened and FUSBasp.USBaspDevice.HasSNWrite;
  sbtnChangeSerial.Enabled := medtSerialNumber.Enabled;
  lblChangeSerial.Enabled := sbtnChangeSerial.Enabled;
  cbxUSBaspDevice.Enabled := not FUSBasp.Connected;
  sbtnRefresh.Enabled := cbxUSBaspDevice.Enabled;
  btnConnect.Enabled := (FUSBasp.USBaspID <> USBaspIDNotFound) and not FUSBasp.Connected;
  btnDisconnect.Enabled := (FUSBasp.USBaspID <> USBaspIDNotFound) and
    not btnConnect.Enabled;
  gbUART.Enabled := (FUSBasp.USBaspID <> USBaspIDNotFound) and
    (FUSBasp.USBaspDevice.HasUart or FUSBasp.USBaspDevice.HasHIDUart) and
    FUSBasp.Connected;
  gbRuntimeSettings.Enabled := gbUART.Enabled;
  gbNoRuntimeSettings.Enabled := not FUSBasp.UARTOpened and gbUART.Enabled;
  btnOpen.Enabled := gbUART.Enabled and not FUSBasp.UARTOpened;
  btnClose.Enabled := gbUART.Enabled and not btnOpen.Enabled;
  edtSend.Enabled := gbUART.Enabled and not btnOpen.Enabled;
  btnSend.Enabled := gbUART.Enabled and not btnOpen.Enabled;

  if FUSBasp.UARTOpened then
    edtSend.SetFocus;
  if btnConnect.Enabled then
    btnConnect.SetFocus;
end;

procedure TfrmMain.AutoScrollHack;
begin
  if ckbAutoScroll.State = cbChecked then
  begin
    /////// HACK TO SCROLL BOTTOM //////////
    mmDisplay.SelStart := Length(mmDisplay.Lines.Text);
    mmDisplay.VertScrollBar.Position := mmDisplay.Lines.Count;
    /////// HACK TO SCROLL BOTTOM //////////
  end;
end;

function TfrmMain.GetTimestamp: string;
begin
  Result := TimeToStr(Now) + ': ';
end;

procedure TfrmMain.NoLineBreak(Data: PtrInt);
var
  RawSerialDataMsg: TRawSerialDataMsg;
  LastLine: integer;
begin
  RawSerialDataMsg := PRawSerialDataMsg(Data)^;
  try
    if (mmDisplay <> nil) and (not Application.Terminated) and
      (RawSerialDataMsg.AsString.Length > 0) then
    begin
      LastLine := mmDisplay.Lines.Count - 1;
      mmDisplay.Lines.BeginUpdate;
      mmDisplay.Lines[LastLine] :=
        mmDisplay.Lines[LastLine] + RawSerialDataMsg.AsString.Substring(0);
      AutoScrollHack;
      mmDisplay.Lines.EndUpdate;
    end;
  finally
    Dispose(PRawSerialDataMsg(Data));
  end;
end;

procedure TfrmMain.LineBreakCRorLF(Data: PtrInt);
var
  RawSerialDataMsg: TRawSerialDataMsg;
  LastLine, i, j: integer;
begin
  RawSerialDataMsg := PRawSerialDataMsg(Data)^;
  try
    if (mmDisplay <> nil) and (not Application.Terminated) and
      (RawSerialDataMsg.AsString.Length > 0) then
    begin
      LastLine := mmDisplay.Lines.Count - 1;
      i := 1;
      j := i;
      mmDisplay.Lines.BeginUpdate;
      while i < RawSerialDataMsg.AsString.Length do
      begin
        if (RawSerialDataMsg.AsString[i] = RawSerialDataMsg.BreakChar) then
        begin
          mmDisplay.Lines[LastLine] :=
            mmDisplay.Lines[LastLine] + RawSerialDataMsg.AsString.Substring(
            j - 1, i - j);
          if ckbTimeStamp.State = cbChecked then
            mmDisplay.Append(GetTimeStamp)
          else
            mmDisplay.Append('');
          LastLine := mmDisplay.Lines.Count - 1;
          Inc(i, 1);
          j := i;
        end
        else
          Inc(i);
      end;
      mmDisplay.Lines[LastLine] :=
        mmDisplay.Lines[LastLine] + RawSerialDataMsg.AsString.Substring(j - 1);
      AutoScrollHack;
      mmDisplay.Lines.EndUpdate;
    end;
  finally
    Dispose(PRawSerialDataMsg(Data));
  end;
end;

procedure TfrmMain.LineBreakCRLF(Data: PtrInt);
var
  RawSerialDataMsg: TRawSerialDataMsg;
  LastLine, i, j: integer;
begin
  RawSerialDataMsg := PRawSerialDataMsg(Data)^;
  try
    if (mmDisplay <> nil) and (not Application.Terminated) and
      (RawSerialDataMsg.AsString.Length > 0) then
    begin
      LastLine := mmDisplay.Lines.Count - 1;
      i := 1;
      j := i;
      if FLastCharReceived = #13 then
        RawSerialDataMsg.AsString := FLastCharReceived + RawSerialDataMsg.AsString;
      mmDisplay.Lines.BeginUpdate;
      while i < RawSerialDataMsg.AsString.Length do
      begin
        if (RawSerialDataMsg.AsString[i] = #13) and
          (RawSerialDataMsg.AsString[i + 1] = #10) then
        begin
          mmDisplay.Lines[LastLine] :=
            mmDisplay.Lines[LastLine] + RawSerialDataMsg.AsString.Substring(
            j - 1, i - j);
          if ckbTimeStamp.State = cbChecked then
            mmDisplay.Append(GetTimeStamp)
          else
            mmDisplay.Append('');
          LastLine := mmDisplay.Lines.Count - 1;
          Inc(i, 2);
          j := i;
        end
        else
          Inc(i);
      end;
      mmDisplay.Lines[LastLine] :=
        mmDisplay.Lines[LastLine] + RawSerialDataMsg.AsString.Substring(j - 1);
      AutoScrollHack;
      mmDisplay.Lines.EndUpdate;
      if RawSerialDataMsg.AsString[RawSerialDataMsg.AsString.Length] = #13 then
        FLastCharReceived := #13
      else
        FLastCharReceived := #0;
    end;
  finally
    Dispose(PRawSerialDataMsg(Data));
  end;
end;

end.
