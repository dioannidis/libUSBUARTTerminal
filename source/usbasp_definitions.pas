unit USBasp_Definitions;

{

  This file is part of Nephelae's Object Pascal FPUSBasp.

  USBasp definitions.

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
  Classes, {libusb,} hidapi;

const
  USBASP_SHARED_VID = $16C0;
  USBASP_SHARED_PID = $05DC;

  USBASP_FUNC_GETCAPABILITIES = 127;

  USBASP_CAP_0_TPI = 1;
  USBASP_CAP_PDI = 16;
  USBASP_CAP_2_SNHIDUPDATE = 32;
  USBASP_CAP_6_UART = 64;
  USBASP_CAP_7_HID_UART = 128;

  USBASP_NO_CAPS = (-4);

  USBASP_CAP_12MHZ_CLOCK = 0;
  USBASP_CAP_16MHZ_CLOCK = 1;
  USBASP_CAP_18MHZ_CLOCK = 2;
  USBASP_CAP_20MHZ_CLOCK = 3;

  PROG_STATE_IDLE = 0;
  PROG_STATE_WRITEFLASH = 1;
  PROG_STATE_READFLASH = 2;
  PROG_STATE_READEEPROM = 3;
  PROG_STATE_WRITEEEPROM = 4;
  PROG_STATE_TPI_READ = 5;
  PROG_STATE_TPI_WRITE = 6;
  PROG_STATE_SET_REPORT = 7;

  UART_STATE_ENABLED = 16;
  UART_STATE_DISABLED = 0;

  // USBasp UART Extension
  // https://github.com/dioannidis/usbasp/blob/master/firmware/usbasp.h

  USBASP_UART_PARITY_MASK = %11;
  USBASP_UART_PARITY_NONE = %00;
  USBASP_UART_PARITY_EVEN = %01;
  USBASP_UART_PARITY_ODD = %10;

  USBASP_UART_STOP_MASK = %100;
  USBASP_UART_STOP_1BIT = %000;
  USBASP_UART_STOP_2BIT = %100;

  USBASP_UART_BYTES_MASK = %111000;
  USBASP_UART_BYTES_5B = %000000;
  USBASP_UART_BYTES_6B = %001000;
  USBASP_UART_BYTES_7B = %010000;
  USBASP_UART_BYTES_8B = %011000;
  USBASP_UART_BYTES_9B = %100000;

type

  { TUSBaspHIDIntf }

  TUSBaspHIDIntf = record
    Device: PHidDevice;
    index: byte;
    Path: string;
    ContainerID: TGUID;
    Serial: string;
    Manufacturer: string;
    Product: string;
    FirmwareVersion: string;
    Capabilities: string;
    VendorID: word;
    ProductID: word;
    InterfaceNumber: integer;
    ReportSize: word;
    PacketCount: byte;
  end;
  PUSBaspHIDIntf = ^TUSBaspHIDIntf;

  { TUSBaspHIDIntfList }

  TUSBaspHIDIntfList = class(TFPList)
  private
    function Get(Index: integer): PUSBaspHIDIntf;
  public
    destructor Destroy; override;
    function Add(Value: PUSBaspHIDIntf): integer;
    procedure FreeItems;
    property Items[Index: integer]: PUSBaspHIDIntf read Get; default;
  end;

  { TUSBasp }

  TUSBasp = record
    Device: pointer; // plibusb_device;
    Handle: pointer; // plibusb_device_handle;
    UARTInterface: PUSBaspHIDIntf;
    MonitorInterface: PUSBaspHIDIntf;
{$ifdef MSWINDOWS}
    ContainerID: TGUID;
{$endif}
{$ifdef UNIX}
  {$ifdef LINUX}
    Path: String[255];
  {$endif}
  {$ifdef BSD}
    {$ifdef FREEBSD}
    Path: String[255];
    {$endif}
    {$ifdef DARWIN}
    {$endif}
  {$endif}
{$endif}
    ProductName: string[255];
    Manufacturer: string[255];
    SerialNumber: string[255];
    FirmwareVersion: string;
    HasUart: boolean;
    HasHIDUart: boolean;
    HasTPI: boolean;
    HasPDI: boolean;
    HasSNWrite: boolean;
    HasMonitorIntf: boolean;
    CrystalOsc: integer;
    Interface0Claimed: boolean;
  end;
  PUSBasp = ^TUSBasp;

  { TUSBaspList }

  TUSBaspList = class(TFPList)
  private
    function Get(Index: integer): PUSBasp;
  public
    destructor Destroy; override;
    function Add(Value: PUSBasp): integer;
    procedure FreeItems;
    property Items[Index: integer]: PUSBasp read Get; default;
  end;

implementation

{ TUSBaspHIDIntfList }

function TUSBaspHIDIntfList.Get(Index: integer): PUSBaspHIDIntf;
begin
  Result := PUSBaspHIDIntf(inherited Get(Index));
end;

destructor TUSBaspHIDIntfList.Destroy;
begin
  FreeItems;
  inherited Destroy;
end;

function TUSBaspHIDIntfList.Add(Value: PUSBaspHIDIntf): integer;
begin
  Result := inherited Add(Value);
end;

procedure TUSBaspHIDIntfList.FreeItems;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Items[i]);
end;

{ TUSBaspList }

function TUSBaspList.Get(Index: integer): PUSBasp;
begin
  Result := PUSBasp(inherited Get(Index));
end;

procedure TUSBaspList.FreeItems;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Items[i]);
end;

destructor TUSBaspList.Destroy;
begin
  FreeItems;
  inherited Destroy;
end;

function TUSBaspList.Add(Value: PUSBasp): integer;
begin
  Result := inherited Add(Value);
end;

end.
