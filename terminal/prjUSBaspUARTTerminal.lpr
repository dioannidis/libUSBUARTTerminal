program prjUSBaspUARTTerminal;

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

uses
{$IFDEF UNIX}
{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}
{$ENDIF}
{$if declared(UseHeapTrace)}
  SysUtils,
{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms,
  FPUSBaspGUIUARTTerminal;

{$R *.res}

begin
{$if declared(UseHeapTrace)}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
{$endIf}
  RequireDerivedFormResource := True;
  Application.Title := 'USBasp HID UART Terminal';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
