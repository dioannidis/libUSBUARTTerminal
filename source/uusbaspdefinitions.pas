unit uUSBaspDefinitions;

{

  This file is part of Object Pascal libUSB UART Terminal for USBasp ( Firmware 1.5 patched ).

  libUSB USBasp UART library definitions.

  Copyright (C) 2020 - 2022 Dimitrios Chr. Ioannidis.
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
  Classes, libusb;

const
  USBASP_SHARED_VID = $16C0;
  USBASP_SHARED_PID = $05DC;

  USBASP_FUNC_GETCAPABILITIES = 127;

  USBASP_CAP_0_TPI            = 1;
  USBASP_CAP_6_UART           = 6;
  USBASP_CAP_7_HID_UART       = 7;

  USBASP_NO_CAPS = (-4);

  // USBasp UART Extension
  // See  https://github.com/akrasuski1/usbasp-uart
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

  USBASP_FUNC_UART_CONFIG     = 60;
  USBASP_FUNC_UART_FLUSHTX    = 61;
  USBASP_FUNC_UART_FLUSHRX    = 62;
  USBASP_FUNC_UART_DISABLE    = 63;
  USBASP_FUNC_UART_TX         = 64;
  USBASP_FUNC_UART_RX         = 65;
  USBASP_FUNC_UART_TX_FREE    = 66;
  USBASP_FUNC_UART_RX_FREE    = 67;

type

  { TUSBaspDevice }

  TUSBaspDevice = record
    USBDevice: plibusb_device;
    Handle: plibusb_device_handle;
    ProductName: string[255];
    Manufacturer: string[255];
    SerialNumber: string[255];
    HasUart: boolean;
    HasHIDUart: boolean;
    HasTPI: boolean;
    FOsc  : Integer;
    Interface0Claimed: boolean;
  end;
  PUSBaspDevice = ^TUSBaspDevice;

  { TUSBaspDeviceList }

  TUSBaspDeviceList = class(TList)
  private
    function Get(Index: integer): PUSBaspDevice;
  public
    destructor Destroy; override;
    function Add(Value: PUSBaspDevice): integer;
    procedure FreeItems;
    property Items[Index: integer]: PUSBaspDevice read Get; default;
  end;

implementation

{ TUSBaspDeviceList }

function TUSBaspDeviceList.Get(Index: integer): PUSBaspDevice;
begin
  Result := PUSBaspDevice(inherited Get(Index));
end;

procedure TUSBaspDeviceList.FreeItems;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    FreeMem(Items[i]);
end;

destructor TUSBaspDeviceList.Destroy;
begin
  FreeItems;
  inherited Destroy;
end;

function TUSBaspDeviceList.Add(Value: PUSBaspDevice): integer;
begin
  Result := inherited Add(Value);
end;

end.

