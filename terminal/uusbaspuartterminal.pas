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

  { TfrmMain }

  TfrmMain = class(TForm)
    btnClearMemo: TButton;
    btnConnect: TButton;
    btnSend: TButton;
    btnDisconnect: TButton;
    cbxBoudRate: TComboBoxEx;
    cbxLineBreak: TComboBoxEx;
    ckbAutoScroll: TCheckBox;
    ckbTimeStamp: TCheckBox;
    edtSend: TEdit;
    gbNoRuntimeSettings: TGroupBox;
    GroupBox2: TGroupBox;
    lblBaud: TLabel;
    lblMemoBufferLines: TLabel;
    lblLineBreak: TLabel;
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
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure ckbAutoScrollChange(Sender: TObject);
    procedure ckbTimeStampChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure medtMemoBufferLinesChange(Sender: TObject);
    procedure mmDisplayChange(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
  private
    FUSBasp: TUSBasp;
    procedure ToggleGUI(ARunning: boolean);
    procedure USBaspTerminated(Sender: TObject);
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

const
  BaudInt: array[0..17] of integer =
    (110, 300, 600, 1200, 2400, 4800, 9600, 14400, 19200,
    38400, 56000, 57600, 115200, 128000, 230400, 256000, 460800, 921600);

{ TfrmMain }

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  //FUSBasp := TUSBasp.Create(BaudInt[cbxBoudRate.ItemIndex + 3],
  //  mmDisplay, TLineBreakMode(cbxLineBreak.ItemIndex), ckbAutoScroll.State =
  //  cbChecked, ckbTimeStamp.State = cbChecked);
  //FUSBasp.OnTerminate := @USBaspTerminated;
  //FUSBasp.FreeOnTerminate := True;
  //FUSBasp.Start;
  FUSBasp.Connect;
  ToggleGUI(True);
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  FUSBasp.Disconnect;
  ToggleGUI(False);
end;

procedure TfrmMain.btnClearMemoClick(Sender: TObject);
begin
  mmDisplay.Clear;
end;

procedure TfrmMain.btnSendClick(Sender: TObject);
begin
  FUSBasp.SendBuffer := edtSend.Text;
end;

procedure TfrmMain.ckbAutoScrollChange(Sender: TObject);
begin
  if Assigned(FUSBasp) then
    FUSBasp.AutoScroll := ckbAutoScroll.State = cbChecked;
end;

procedure TfrmMain.ckbTimeStampChange(Sender: TObject);
begin
  if Assigned(FUSBasp) then
    FUSBasp.TimeStamp := ckbTimeStamp.State = cbChecked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  FUSBasp := TUSBasp.Create();
  if FileExists(ChangeFileExt(Application.ExeName, '.xml')) then
  begin
    Position := poDesigned;
    DefaultMonitor := dmActiveForm;
  end;
  USBaspUARTAbout.ShowSplash;
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

procedure TfrmMain.ToggleGUI(ARunning: boolean);
begin
  if ARunning then
  begin
    btnConnect.Enabled := False;
    gbNoRuntimeSettings.Enabled := False;
    btnDisconnect.Enabled := True;
    edtSend.Enabled := True;
    btnSend.Enabled := True;
    edtSend.SetFocus;
  end
  else
  begin
    btnConnect.Enabled := True;
    gbNoRuntimeSettings.Enabled := True;
    btnDisconnect.Enabled := False;
    edtSend.Enabled := False;
    btnSend.Enabled := False;
    btnConnect.SetFocus;
  end;
end;

procedure TfrmMain.USBaspTerminated(Sender: TObject);
begin
  FUSBasp := nil;
  ToggleGUI(False);
end;

end.
