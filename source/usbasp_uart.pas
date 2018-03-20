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

type
  USBaspUART = record
    USBHandle: plibusb_device_handle;
  end;

function usbasp_uart_open(var USBasp: USBaspUART): integer;

implementation

type
  pplibusb_device = ^plibusb_device;
  ppplibusb_device = ^pplibusb_device;

function usbasp_uart_open(var USBasp: USBaspUART): integer;
var
  USBContext: plibusb_context;
  USBDevice: plibusb_device;
  USBDeviceDescriptor: libusb_device_descriptor;
  USBDeviceList: ppplibusb_device;
  iResult, i, USBDeviceListCount: integer;
  StrBuffer: array [0..255] of byte;
  StrTemp: string;
begin
  Result := USB_ERROR_NOTFOUND;
  iResult := libusb_init(USBContext);
  libusb_set_debug(USBContext, 3);
  USBDeviceListCount := libusb_get_device_list(USBContext, plibusb_device(USBDeviceList));
  try
    for i := 0 to USBDeviceListCount - 1 do
    begin
      USBDevice := @USBDeviceList[i]^;
      libusb_get_device_descriptor(USBDevice, USBDeviceDescriptor);
      if (USBDeviceDescriptor.idVendor = USBASP_SHARED_VID) and (USBDeviceDescriptor.idProduct = USBASP_SHARED_PID) then
      begin
        if libusb_Open(USBDevice, USBasp.USBHandle) = 0 then
        begin
          iResult := libusb_get_string_descriptor_ascii(USBasp.USBHandle, USBDeviceDescriptor.iProduct, @StrBuffer[0], Length(StrBuffer));
          SetString(StrTemp, PChar(@StrBuffer[0]), iResult);
          if StrTemp <> 'USBasp' then
          begin
            USBasp.USBHandle := nil;
            libusb_close(USBasp.USBHandle);
            continue;
          end;
          iResult := libusb_get_string_descriptor_ascii(USBasp.USBHandle, USBDeviceDescriptor.iManufacturer, @StrBuffer[0], Length(StrBuffer));
          SetString(StrTemp, PChar(@StrBuffer[0]), iResult);
          if StrTemp <> 'www.fischl.de' then
          begin
            USBasp.USBHandle := nil;
            libusb_close(USBasp.USBHandle);
            continue;
          end;
          iResult := libusb_get_string_descriptor_ascii(USBasp.USBHandle, USBDeviceDescriptor.iSerialNumber, @StrBuffer[0], Length(StrBuffer));
          SetString(StrTemp, PChar(@StrBuffer[0]), iResult);
          if StrTemp <> 'v1.5.1' then
          begin
            USBasp.USBHandle := nil;
            libusb_close(USBasp.USBHandle);
            continue;
          end;
          break;
        end;
      end;
    end;
  finally
    libusb_free_device_list(plibusb_device(USBDeviceList), 1);
    libusb_exit(USBContext);
  end;
  if USBasp.USBHandle <> nil then
    Result := 0;
end;

end.
