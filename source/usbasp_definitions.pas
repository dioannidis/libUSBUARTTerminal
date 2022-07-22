unit USBasp_Definitions;

{

  This file is part of Nephelae's Object Pascal FPUSBasp.

  USBasp definitions.

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
  Classes, libusb, hidapi;

const
  USBASP_SHARED_VID = $16C0;
  USBASP_SHARED_PID = $05DC;

  USBASP_FUNC_GETCAPABILITIES = 127;

  USBASP_CAP_0_TPI            = 1;
  USBASP_CAP_6_UART           = 6;
  USBASP_CAP_7_HID_UART       = 7;

  USBASP_NO_CAPS = (-4);

  USBASP_CAP_12MHZ_CLOCK      = 0;
  USBASP_CAP_16MHZ_CLOCK      = 1;
  USBASP_CAP_18MHZ_CLOCK      = 2;
  USBASP_CAP_20MHZ_CLOCK      = 3;

  // USBasp UART Extension
  // https://github.com/dioannidis/usbasp/blob/master/firmware/usbasp.h
  //
  USBASP_UART_PARITY_MASK     = %11;
  USBASP_UART_PARITY_NONE     = %00;
  USBASP_UART_PARITY_EVEN     = %01;
  USBASP_UART_PARITY_ODD      = %10;

  USBASP_UART_STOP_MASK       = %100;
  USBASP_UART_STOP_1BIT       = %000;
  USBASP_UART_STOP_2BIT       = %100;

  USBASP_UART_BYTES_MASK      = %111000;
  USBASP_UART_BYTES_5B        = %000000;
  USBASP_UART_BYTES_6B        = %001000;
  USBASP_UART_BYTES_7B        = %010000;
  USBASP_UART_BYTES_8B        = %011000;
  USBASP_UART_BYTES_9B        = %100000;

type

  { TUSBasp_HIDDevice }

  TUSBasp_HIDDevice = record
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
  PUSBasp_HIDDevice = ^TUSBasp_HIDDevice;

  { TUSBasp_HIDDeviceList }

  TUSBasp_HIDDeviceList = class(TFPList)
  private
    function Get(Index: integer): PUSBasp_HIDDevice;
  public
    destructor Destroy; override;
    function Add(Value: PUSBasp_HIDDevice): integer;
    procedure FreeItems;
    property Items[Index: integer]: PUSBasp_HIDDevice read Get; default;
  end;

  { TUSBasp_USBDevice }

  TUSBasp_USBDevice = record
    Device: plibusb_device;
    Handle: plibusb_device_handle;
    HidDevice: PUSBasp_HIDDevice;
    MonitorHidDevice: PUSBasp_HIDDevice;
    ContainerID: TGUID;
    ProductName: string[255];
    Manufacturer: string[255];
    SerialNumber: string[255];
    HasUart: boolean;
    HasHIDUart: boolean;
    HasTPI: boolean;
    CrystalOsc: Integer;
    Interface0Claimed: boolean;
  end;
  PUSBasp_USBDevice = ^TUSBasp_USBDevice;

  { TUSBasp_USBDeviceList }

  TUSBasp_USBDeviceList = class(TFPList)
  private
    function Get(Index: integer): PUSBasp_USBDevice;
  public
    destructor Destroy; override;
    function Add(Value: PUSBasp_USBDevice): integer;
    procedure FreeItems;
    property Items[Index: integer]: PUSBasp_USBDevice read Get; default;
  end;

implementation

{ TUSBasp_HIDDeviceList }

function TUSBasp_HIDDeviceList.Get(Index: integer): PUSBasp_HIDDevice;
begin
  Result := PUSBasp_HIDDevice(inherited Get(Index));
end;

destructor TUSBasp_HIDDeviceList.Destroy;
begin
  FreeItems;
  inherited Destroy;
end;

function TUSBasp_HIDDeviceList.Add(Value: PUSBasp_HIDDevice): integer;
begin
  Result := inherited Add(Value);
end;

procedure TUSBasp_HIDDeviceList.FreeItems;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Items[i]);
end;

{ TUSBasp_USBDeviceList }

function TUSBasp_USBDeviceList.Get(Index: integer): PUSBasp_USBDevice;
begin
  Result := PUSBasp_USBDevice(inherited Get(Index));
end;

procedure TUSBasp_USBDeviceList.FreeItems;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Items[i]);
end;

destructor TUSBasp_USBDeviceList.Destroy;
begin
  FreeItems;
  inherited Destroy;
end;

function TUSBasp_USBDeviceList.Add(Value: PUSBasp_USBDevice): integer;
begin
  Result := inherited Add(Value);
end;

end.
