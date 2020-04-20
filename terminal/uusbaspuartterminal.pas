unit uusbaspuartterminal;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, ComboEx, ExtCtrls, MaskEdit, XMLPropStorage, usplashabout, uusbasp;

type

  TLineBreakMode = (lbmNoLineBreak, lbmCR, lbmLF, lbmCRLF);

  { TfrmMain }

  TfrmMain = class(TForm)
    btnClearMemo: TButton;
    btnClose: TButton;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnOpen: TButton;
    btnSend: TButton;
    cbxBaudRate: TComboBoxEx;
    cbxLineBreak: TComboBoxEx;
    cbxUSBaspDevice: TComboBox;
    ckbAutoScroll: TCheckBox;
    ckbTimeStamp: TCheckBox;
    edtSend: TEdit;
    gbNoRuntimeSettings: TGroupBox;
    gbRuntimeSettings: TGroupBox;
    gbUART: TGroupBox;
    lblBaud: TLabel;
    lblLineBreak: TLabel;
    lblMemoBufferLines: TLabel;
    lblUSBaspDevice: TLabel;
    AppMainMenu: TMainMenu;
    medtMemoBufferLines: TMaskEdit;
    mmDisplay: TMemo;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miAbout: TMenuItem;
    USBaspUARTAbout: TSplashAbout;
    AppStatusBar: TStatusBar;
    AppXMLPropStorage: TXMLPropStorage;
    procedure btnClearMemoClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure cbxUSBaspDeviceCloseUp(Sender: TObject);
    procedure cbxUSBaspDeviceDropDown(Sender: TObject);
    procedure edtSendKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure medtMemoBufferLinesChange(Sender: TObject);
    procedure mmDisplayChange(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
  private
    FUSBasp: TUSBasp;
    FLastCharReceived: char;
    procedure ToggleGUI;

    procedure USBaspUARTReceiveProcessing(const AMsg: string);
    procedure NoLineBreak(Data: PtrInt);
    procedure LineBreakCRLF(Data: PtrInt);

    procedure AutoScrollHack;
    function GetTimestamp: string;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnOpenClick(Sender: TObject);
var
  s: string;
begin
  if FUSBasp.Connected and FUSBasp.SupportUART and not FUSBasp.UARTOpened then
    FUSBasp.UARTOpen(TSerialBaudRate[cbxBaudRate.ItemIndex]);
  ToggleGUI;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  if FUSBasp.Connected and FUSBasp.SupportUART and FUSBasp.UARTOpened then
    FUSBasp.UARTClose;
  ToggleGUI;
end;

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

procedure TfrmMain.cbxUSBaspDeviceCloseUp(Sender: TObject);
begin
  if FUSBasp.USBaspDevices.Count - 1 >= 0 then
  begin
    FUSBasp.USBaspID := cbxUSBaspDevice.ItemIndex;
    AppStatusBar.SimpleText :=
      'Product: [' + FUSBasp.USBaspDevice.ProductName + '] Manufacturer: [' +
      FUSBasp.USBaspDevice.Manufacturer + '] Serial number: [' +
      FUSBasp.USBaspDevice.SerialNumber + '] TPI: [' + BoolToStr(FUSBasp.USBaspDevice.HasTPI, 'On', 'Off') +
      '] UART: [' +BoolToStr(FUSBasp.USBaspDevice.HasTPI, 'On', 'Off') +']';
  end
  else
    AppStatusBar.SimpleText := 'No USBasp Device Found';
  ToggleGUI;
end;

procedure TfrmMain.cbxUSBaspDeviceDropDown(Sender: TObject);
var
  i: byte;
begin
  if FUSBasp.Connected then
    Exit;
  cbxUSBaspDevice.Items.BeginUpdate;
  cbxUSBaspDevice.Items.Clear;
  FUSBasp.LoadUSBaspDevices;
  if FUSBasp.USBaspDevices.Count - 1 >= 0 then
  begin
    for i := 0 to FUSBasp.USBaspDevices.Count - 1 do
      cbxUSBaspDevice.AddItem(FUSBasp.USBaspDevices[i]^.ProductName +
        ':' + FUSBasp.USBaspDevices[i]^.SerialNumber + ' [' +
        FUSBasp.USBaspDevices[i]^.Manufacturer + ']', nil);
    cbxUSBaspDevice.ItemIndex := 0;
  end
  else
  begin
    cbxUSBaspDevice.AddItem('No USBasp Device Found', nil);
    cbxUSBaspDevice.ItemIndex := -1;
  end;
  cbxUSBaspDevice.Items.EndUpdate;
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
begin
  FUSBasp.SendBuffer := edtSend.Text;
  edtSend.Text := '';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  FLastCharReceived := #0;

  FUSBasp := TUSBasp.Create();
  FUSBasp.OnUARTReceive := @USBaspUARTReceiveProcessing;

  if FileExists(ChangeFileExt(Application.ExeName, '.xml')) then
  begin
    Position := poDesigned;
    DefaultMonitor := dmActiveForm;
  end;

  USBaspUARTAbout.ShowSplash;
  ToggleGUI;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
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
  USBaspUARTAbout.ShowAbout;
end;

procedure TfrmMain.ToggleGUI;
begin
  cbxUSBaspDevice.Enabled := not FUSBasp.Connected;
  btnConnect.Enabled := (FUSBasp.USBaspID <> 255) and not FUSBasp.Connected;
  btnDisconnect.Enabled := (FUSBasp.USBaspID <> 255) and not btnConnect.Enabled;
  gbUART.Enabled := (FUSBasp.USBaspID <> 255) and FUSBasp.SupportUART and
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

// This method is running
// from inside the USBasp Receive thread.
procedure TfrmMain.USBaspUARTReceiveProcessing(const AMsg: string);
var
  RawSerialDataMsg: PRawSerialDataMsg;
begin
  New(RawSerialDataMsg);
  RawSerialDataMsg^.AsString := AMsg;
  case cbxLineBreak.ItemIndex of
    0: Application.QueueAsyncCall(@NoLineBreak, PtrInt(RawSerialDataMsg));
    //  lbmCR: Application.QueueAsyncCall(@LineBreakCR, PtrInt(RawSerialDataMsg));
    //  lbmLF: Application.QueueAsyncCall(@LineBreakLF, PtrInt(RawSerialDataMsg));
    3: Application.QueueAsyncCall(@LineBreakCRLF, PtrInt(RawSerialDataMsg))
  end;
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

end.
