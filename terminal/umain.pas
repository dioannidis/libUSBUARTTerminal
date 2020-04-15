unit umain;

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
  ComCtrls, ComboEx, ExtCtrls, MaskEdit, uusbasp;

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
    MainMenu1: TMainMenu;
    medtMemoBufferLines: TMaskEdit;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    StatusBar1: TStatusBar;
    procedure btnClearMemoClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure ckbAutoScrollChange(Sender: TObject);
    procedure ckbTimeStampChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure medtMemoBufferLinesChange(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    FUSBaspThread: TUSBaspThread;
    procedure ToggleGUI(ARunning: boolean);
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
  FUSBaspThread := TUSBaspThread.Create(BaudInt[cbxBoudRate.ItemIndex + 3],
    Memo1, TLineBreakMode(cbxLineBreak.ItemIndex), ckbAutoScroll.State =
    cbChecked, ckbTimeStamp.State = cbChecked);
  FUSBaspThread.FreeOnTerminate := True;
  FUSBaspThread.Start;
  ToggleGUI(True);
end;

procedure TfrmMain.btnClearMemoClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TfrmMain.btnSendClick(Sender: TObject);
begin
  FUSBaspThread.SendBuffer := edtSend.Text;
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  if Assigned(FUSBaspThread) then
  begin
    FUSBaspThread.Terminate;
    FUSBaspThread.WaitFor;
    FUSBaspThread := nil;
    ToggleGUI(False);
  end;
end;

procedure TfrmMain.ckbAutoScrollChange(Sender: TObject);
begin
  if Assigned(FUSBaspThread) then
    FUSBaspThread.AutoScroll := ckbAutoScroll.State = cbChecked;
end;

procedure TfrmMain.ckbTimeStampChange(Sender: TObject);
begin
  if Assigned(FUSBaspThread) then
    FUSBaspThread.TimeStamp := ckbTimeStamp.State = cbChecked;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  btnDisconnectClick(Self);
  CanClose := not Assigned(FUSBaspThread);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
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

procedure TfrmMain.Memo1Change(Sender: TObject);
begin
  if Memo1.Lines.Count > StrToInt(medtMemoBufferLines.Text) + 10 then
  begin
    Memo1.Lines.BeginUpdate;
    while Memo1.Lines.Count > StrToInt(medtMemoBufferLines.Text) do
      Memo1.Lines.Delete(0);
    Memo1.Lines.EndUpdate;
  end;
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

end.
