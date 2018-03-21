unit usbasp_uart;

{

  This file is part of Object Pascal libUSB UART Terminal for USBasp ( Firmware 1.5 patched ).

  libUSB UART Terminal types and helper functions.

  Copyright (C) 2018 Dimitrios Chr. Ioannidis.
    Nephelae - https://www.nephelae.eu

  https://www.nephelae.eu/USBaspUARTTerminal/

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
  libusb;

const
  USB_ERROR_NOTFOUND = 1;
  USB_ERROR_ACCESS = 2;
  USB_ERROR_IO = 3;

  USBASP_SHARED_VID = $16C0;
  USBASP_SHARED_PID = $05DC;

  USBASP_FUNC_UART_CONFIG  = 60;
  USBASP_FUNC_UART_FLUSHTX = 61;
  USBASP_FUNC_UART_FLUSHRX = 62;
  USBASP_FUNC_UART_DISABLE = 63;
  USBASP_FUNC_UART_TX      = 64;
  USBASP_FUNC_UART_RX      = 65;
  USBASP_FUNC_UART_TX_FREE = 66;
  USBASP_FUNC_UART_RX_FREE = 67;

  USBASP_FUNC_GETCAPABILITIES = 127;

  USBASP_CAP_6_UART = (1 shl 6);

  USBASP_UART_PARITY_MASK  = %11;
  USBASP_UART_PARITY_NONE  = %00;
  USBASP_UART_PARITY_EVEN  = %01;
  USBASP_UART_PARITY_ODD   = %10;

  USBASP_UART_STOP_MASK    = %100;
  USBASP_UART_STOP_1BIT    = %000;
  USBASP_UART_STOP_2BIT    = %100;

  USBASP_UART_BYTES_MASK   = %111000;
  USBASP_UART_BYTES_5B     = %000000;
  USBASP_UART_BYTES_6B     = %001000;
  USBASP_UART_BYTES_7B     = %010000;
  USBASP_UART_BYTES_8B     = %011000;
  USBASP_UART_BYTES_9B     = %100000;

  USBASP_NO_CAPS = (-4);

type
  USBaspUART = record
    Handle: plibusb_device_handle;
    Context: plibusb_context;
  end;
  PUSBaspUART = ^USBaspUART;

function usbasp_uart_open(AUSBasp: PUSBaspUART): integer;
procedure usbasp_uart_disable(AUSBasp: PUSBaspUART);

implementation

type
  pplibusb_device = ^plibusb_device;
  ppplibusb_device = ^pplibusb_device;
  
var
  locDummy: Array [0..3] of Char;

function usbasp_uart_transmit(AUSBasp: PUSBaspUART; AReceive: uint8_t; AFunctionId: uint8_t; ASend: Array of Char;
  var ABuffer: Array of Char; ABufferSize: uint16_t): integer;
var
  Res: Integer;
begin
  FillChar(ABuffer, SizeOf(ABuffer), 0);
  Result := libusb_control_transfer(AUSBasp^.Handle,
    (byte(LIBUSB_REQUEST_TYPE_VENDOR) or byte(LIBUSB_RECIPIENT_DEVICE) or (AReceive shl 7)) and $FF, AFunctionId,
    ((byte(ASend[1]) shl 8) or byte(ASend[0])), ((byte(ASend[3]) shl 8) or byte(ASend[2])), @ABuffer[0], ABufferSize, 5000);
end;

function usbasp_uart_open(AUSBasp: PUSBaspUART): integer;

  function CompareDescriptor(AUSBHandle:plibusb_device_handle; ADescriptor: uint8_t; AStringValue: string): boolean;
  var
    iStrLength: integer;
    StrBuffer: array [0..255] of byte;
    StrTemp: string;
  begin
    iStrLength := libusb_get_string_descriptor_ascii(AUSBHandle, ADescriptor, @StrBuffer[0], Length(StrBuffer));
    SetString(StrTemp, PChar(@StrBuffer[0]), iStrLength);
    Result := StrTemp <> AStringValue;
  end;

var
  USBDevice: plibusb_device;
  USBDeviceDescriptor: libusb_device_descriptor;
  USBDeviceList: ppplibusb_device;
  iResult, i, USBDeviceListCount: integer;
begin
  Result := USB_ERROR_NOTFOUND;
  AUSBasp^.Handle := nil;
  iResult := libusb_init(AUSBasp^.Context);
  libusb_set_debug(AUSBasp^.Context, 3);
  USBDeviceListCount := libusb_get_device_list(AUSBasp^.Context, plibusb_device(USBDeviceList));
  try
    for i := 0 to USBDeviceListCount - 1 do
    begin
      USBDevice := @USBDeviceList[i]^;
      libusb_get_device_descriptor(USBDevice, USBDeviceDescriptor);
      if (USBDeviceDescriptor.idVendor = USBASP_SHARED_VID) and (USBDeviceDescriptor.idProduct = USBASP_SHARED_PID) then
      begin
        if libusb_Open(USBDevice, AUSBasp^.Handle) = 0 then
        begin
          if CompareDescriptor(AUSBasp^.Handle, USBDeviceDescriptor.iProduct, 'USBasp') then
          begin
            libusb_close(AUSBasp^.Handle);
            AUSBasp^.Handle := nil;
            continue;
          end;
          if CompareDescriptor(AUSBasp^.Handle, USBDeviceDescriptor.iManufacturer, 'www.fischl.de') then
          begin
            libusb_close(AUSBasp^.Handle);
            AUSBasp^.Handle := nil;
            continue;
          end;
          break;
        end;
      end;
    end;
  finally
    libusb_free_device_list(plibusb_device(USBDeviceList), 1);
  end;
  if AUSBasp^.Handle <> nil then
  begin
    if libusb_kernel_driver_active(AUSBasp^.Handle, 0) = 1 then
      iResult := libusb_detach_kernel_driver(AUSBasp^.Handle, 0);
    iResult := libusb_claim_interface(AUSBasp^.Handle, 0);
    Result := 0;
  end;
end;

procedure usbasp_uart_disable(AUSBasp: PUSBaspUART);
var
  iResult: integer;
begin
  usbasp_uart_transmit(AUSBasp, 1, USBASP_FUNC_UART_DISABLE, locDummy, locDummy, 0);
  iResult := libusb_release_interface(AUSBasp^.Handle, 0);
  libusb_close(AUSBasp^.Handle);
  libusb_exit(AUSBasp^.Context);
end;

initialization
  FillChar(locDummy, SizeOf(locDummy), 0);
  
end.
