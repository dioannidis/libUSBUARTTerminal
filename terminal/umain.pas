unit umain;

{

  This file is part of Object Pascal libUSB UART Terminal for USBasp ( Firmware 1.5 patched ).

  libUSB UART Terminal.

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
  ComCtrls, ComboEx, usbasp_uart;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnConnect: TButton;
    btnSend: TButton;
    btnDisconnect: TButton;
    cbxBoudRate: TComboBoxEx;
    edtSend: TEdit;
    lblBaud: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    StatusBar1: TStatusBar;
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure Memo1Change(Sender: TObject);
    procedure Memo1DblClick(Sender: TObject);
  private
    USBasp: USBaspUART;
    FIsRunning, FStopReading: boolean;
    procedure ToggleGUI(AConnect: boolean);
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
var
  StrBuffer: array [0..253] of byte;
  tmpData: TBytes;
  iStrLength, tmpPos: integer;
  StrLines: TStringArray;
  i: byte;
begin
  if FIsRunning then
    Exit;
  ToggleGUI(True);
  usbasp_uart_config(USBasp, BaudInt[cbxBoudRate.ItemIndex + 3],
    USBASP_UART_PARITY_NONE or USBASP_UART_BYTES_8B or USBASP_UART_STOP_1BIT);
  tmpPos := 0;
  repeat
    if tmpPos = 0 then
      SetLength(tmpData, 8192);
    iStrLength := usbasp_uart_read(USBasp, StrBuffer, Length(StrBuffer));
    if iStrLength = 0 then
      Application.ProcessMessages;
    if iStrLength > 0 then
    begin
      Move(StrBuffer[0], tmpData[tmpPos], iStrLength);
      Inc(tmpPos, iStrLength);
    end
    else
    if tmpPos <> 0 then
    begin
      StrLines := TEncoding.ASCII.GetAnsiString(tmpData).Split([#10#13]);
      if not StrLines[0].IsEmpty then
        Memo1.Lines[Memo1.Lines.Count - 1] :=
          Memo1.Lines[Memo1.Lines.Count - 1] + StrLines[0];
      for i := 1 to High(StrLines) do
        Memo1.Append(StrLines[i]);
      tmpPos := 0;
      tmpData := nil;
    end;
  until FStopReading;
  usbasp_uart_disable(USBasp);
end;

procedure TfrmMain.btnSendClick(Sender: TObject);
var
  StrBuffer: array of byte;
  tmpData: TBytes;
  iStrLength: integer;
begin
  if not FIsRunning then
    Exit;
  FillChar(StrBuffer, SizeOf(StrBuffer), 0);
  tmpData := BytesOf(edtSend.Text);
  SetLength(StrBuffer, Length(tmpData));
  Move(tmpData[0], StrBuffer[0], Length(tmpData));
  iStrLength := usbasp_uart_write(USBasp, StrBuffer, Length(StrBuffer));
  Application.ProcessMessages;
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  ToggleGUI(False);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    ToggleGUI(False);
end;

procedure TfrmMain.Memo1Change(Sender: TObject);
begin
  if Memo1.Lines.Count > 300 then
    Memo1.Clear;
end;

procedure TfrmMain.Memo1DblClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TfrmMain.ToggleGUI(AConnect: boolean);
begin
  if AConnect then
  begin
    FIsRunning := True;
    FStopReading := False;
    btnConnect.Enabled := False;
    btnDisconnect.Enabled := True;
    btnSend.Enabled := True;
  end
  else
  begin
    FIsRunning := False;
    FStopReading := True;
    btnConnect.Enabled := True;
    btnDisconnect.Enabled := False;
    btnSend.Enabled := False;
  end;
end;

end.
