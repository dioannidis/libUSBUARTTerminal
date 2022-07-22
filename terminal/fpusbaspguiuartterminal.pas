unit FPUSBaspGUIUARTTerminal;

{

  This file is part of Nephelae's Object Pascal FPUSBasp.

  LIBUSB/HIDAPI USBasp UART Terminal GUI.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, ComboEx, ExtCtrls, MaskEdit, XMLPropStorage, Buttons, DateUtils,
  USBasp, SPSCRingBuffer;

type

  TRawSerialDataMsg = record
    AsString: string;
    BreakChar: char;
  end;
  PRawSerialDataMsg = ^TRawSerialDataMsg;

  { TThreadMonitor }

  TThreadMonitor = class(TThread)
  private
    FBuffer: TSPSCRingBuffer;
    FData: TByteArray;
    FDataCount: integer;
    procedure DoMonitor;
  protected
    procedure Execute; override;
  public
    constructor Create(const ABuffer: TSPSCRingBuffer); reintroduce;
  end;

  { TThreadUARTRead }

  TThreadUARTRead = class(TThread)
  private
    FBuffer: TSPSCRingBuffer;
    FSerialData: TByteArray;
    FSerialDataCount: integer;
    procedure DoUARTReceiveProcessing;
  protected
    procedure Execute; override;
  public
    constructor Create(const ABuffer: TSPSCRingBuffer); reintroduce;
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
    lblMonitor: TLabel;
    lblBaud: TLabel;
    lblStopBits: TLabel;
    lblParity: TLabel;
    lblDataBits: TLabel;
    lblLineBreak: TLabel;
    lblMemoBufferLines: TLabel;
    lblUSBaspDevice: TLabel;
    AppMainMenu: TMainMenu;
    medtMemoBufferLines: TMaskEdit;
    mmDisplay: TMemo;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miAbout: TMenuItem;
    sbtnRefresh: TSpeedButton;
    AppStatusBar: TStatusBar;
    AppXMLPropStorage: TXMLPropStorage;
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
    procedure mmDisplayChange(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure sbtnRefreshClick(Sender: TObject);
  private
    FUSBasp: TUSBasp;
    FLastCharReceived: char;
    FUARTWantedState: boolean;
    FUARTLastState: TDateTime;

    FThreadUARTRead: TThreadUARTRead;
    FSendSerialData: TBytes;

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
  RawSerialDataMsg: PRawSerialDataMsg;
begin
  if (frmMain.FUSBasp.Connected) and (not Application.Terminated)  then
  begin
    New(RawSerialDataMsg);
    case FData[0] of
      0: RawSerialDataMsg^.AsString := 'IDLE';
      1: RawSerialDataMsg^.AsString := 'WRITEFLASH';
      2: RawSerialDataMsg^.AsString := 'READFLASH';
      3: RawSerialDataMsg^.AsString := 'READEEPROM';
      4: RawSerialDataMsg^.AsString := 'WRITEEEPROM';
      5: RawSerialDataMsg^.AsString := 'TPI_READ';
      6: RawSerialDataMsg^.AsString := 'TPI_WRITE';
      7: RawSerialDataMsg^.AsString := 'UART_COM';
    end;
    Application.QueueAsyncCall(@frmMain.USBaspMonitor, PtrUInt(RawSerialDataMsg));
  end;
end;

procedure TThreadMonitor.Execute;
begin
  repeat
    FDataCount := FBuffer.Read(FData, SizeOf(TByteArray));
    if (FDataCount > 0) and (not Application.Terminated)  then
      Synchronize(@DoMonitor)
    else
      Sleep(2);
  until Terminated;
end;

constructor TThreadMonitor.Create(const ABuffer: TSPSCRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
end;

{ TThreadUARTRead }

procedure TThreadUARTRead.DoUARTReceiveProcessing;
var
  RawSerialDataMsg: PRawSerialDataMsg;
begin
  if (frmMain.FUSBasp.UARTOpened) and (not Application.Terminated)  then
  begin
    New(RawSerialDataMsg);
    RawSerialDataMsg^.AsString :=
      TEncoding.ASCII.GetString(FSerialData, 0, FSerialDataCount);
    case frmMain.cbxLineBreak.ItemIndex of
      0: Application.QueueAsyncCall(@frmMain.NoLineBreak, PtrUInt(RawSerialDataMsg));
      1:
      begin
        RawSerialDataMsg^.BreakChar := #13;
        Application.QueueAsyncCall(@frmMain.LineBreakCRorLF, PtrUInt(RawSerialDataMsg));
      end;
      2:
      begin
        RawSerialDataMsg^.BreakChar := #10;
        Application.QueueAsyncCall(@frmMain.LineBreakCRorLF, PtrUInt(RawSerialDataMsg));
      end;
      3: Application.QueueAsyncCall(@frmMain.LineBreakCRLF, PtrUInt(RawSerialDataMsg))
    end;
  end;
end;

procedure TThreadUARTRead.Execute;
begin
  repeat
    FSerialDataCount := FBuffer.Read(FSerialData, SizeOf(TByteArray));
    if (FSerialDataCount > 0) and (not Application.Terminated) then
      Synchronize(@DoUARTReceiveProcessing)
    else
      Sleep(2);
  until Terminated;
end;

constructor TThreadUARTRead.Create(const ABuffer: TSPSCRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
end;

{ TfrmMain }

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  FUSBasp.Connect(cbxUSBaspDevice.ItemIndex);
  ToggleGUI;
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  FUSBasp.Disconnect;
  ToggleGUI;
end;

procedure TfrmMain.cbxUSBaspDeviceChange(Sender: TObject);
begin
  if FUSBasp.USBaspDevices.Count > 0 then
  begin
    FUSBasp.USBaspID := cbxUSBaspDevice.ItemIndex;
    AppStatusBar.SimpleText :=
      'Product: [' + FUSBasp.USBaspDevice.ProductName + '] Manufacturer: [' +
      FUSBasp.USBaspDevice.Manufacturer + '] Serial number: [' +
      FUSBasp.USBaspDevice.SerialNumber + '] Crystal: [' +
      FUSBasp.USBaspDevice.CrystalOsc.ToString() + ' Hz] TPI: [' +
      BoolToStr(FUSBasp.USBaspDevice.HasTPI, 'On', 'Off') + '] UART: [' +
      BoolToStr(FUSBasp.USBaspDevice.HasUart, 'On', 'Off') + '] HID UART: [' +
      BoolToStr(FUSBasp.USBaspDevice.HasHIDUart, 'On', 'Off') + ']';
  end
  else
    AppStatusBar.SimpleText := 'No USBasp Device Found';
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

procedure TfrmMain.btnSendClick(Sender: TObject);
var
  SendCount: PtrUInt;
begin
  SendCount := 0;
  case cbxLineBreak.ItemIndex of
    1: edtSend.Text := edtSend.Text + #13;
    2: edtSend.Text := edtSend.Text + #10;
    3: edtSend.Text := edtSend.Text + #13#10;
  end;
  FSendSerialData := TEncoding.ASCII.GetBytes(edtSend.Text);
  while SendCount < Length(FSendSerialData) do
    SendCount := SendCount + FUSBasp.TransmitBuffer.Write(
      FSendSerialData[0 + SendCount], Length(FSendSerialData) - SendCount);
  edtSend.Text := '';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  FLastCharReceived := #0;

  FUSBasp := TUSBasp.Create();
  FThreadUARTRead := TThreadUARTRead.Create(FUSBasp.ReceiveBuffer);
  FThreadMonitor := TThreadMonitor.Create(FUSBasp.MonitorBuffer);

  if FileExists(ChangeFileExt(Application.ExeName, '.xml')) then
  begin
    Position := poDesigned;
    DefaultMonitor := dmActiveForm;
  end;

  ToggleGUI;

  FUARTWantedState := False;
  FUARTLastState := Now;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FThreadMonitor.Terminate;
  FThreadMonitor.WaitFor;
  FreeAndNil(FThreadMonitor);

  FThreadUARTRead.Terminate;
  FThreadUARTRead.WaitFor;
  FreeAndNil(FThreadUARTRead);

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

procedure TfrmMain.miAboutClick(Sender: TObject);
begin
  //USBaspUARTAbout.ShowAbout;
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
        ' [' + FUSBasp.USBaspDevices[i]^.Manufacturer + ']', nil);
    cbxUSBaspDevice.ItemIndex := 0;
    cbxUSBaspDeviceChange(self);
  end
  else
  begin
    cbxUSBaspDevice.AddItem('No USBasp Device Found', nil);
    cbxUSBaspDevice.ItemIndex := 0;
    AppStatusBar.SimpleText := 'No USBasp Device Found';
  end;
  cbxUSBaspDevice.Items.EndUpdate;
end;

procedure TfrmMain.USBaspMonitor(Data: PtrInt);
var
  RawSerialDataMsg: TRawSerialDataMsg;
  tmp: int64;
begin
  RawSerialDataMsg := PRawSerialDataMsg(Data)^;
  try
    lblMonitor.Caption := RawSerialDataMsg.AsString;
    if FUARTWantedState then
    begin
      tmp := MilliSecondsBetween(FUARTLastState, Now);
      if (tmp > 100) and (lblMonitor.Caption = 'IDLE') then
      begin
        if not FUSBasp.UARTOpened then
          FUSBasp.UARTOpen(TUARTBaudRate[cbxBaudRate.ItemIndex],
            TUARTDataBits[cbxDataBits.ItemIndex],
            TUARTParity[cbxParity.ItemIndex], TUARTStopBit[cbxStopBits.ItemIndex]);
      end
      else
      if FUSBasp.UARTOpened then
      begin
        FUARTLastState := Now;
        FUSBasp.UARTClose;
      end;
    end;
  finally
    Dispose(PRawSerialDataMsg(Data));
  end;
end;

procedure TfrmMain.ToggleGUI;
begin
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
    mmDisplay.VertScrollBar.Position := 1000000;
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
