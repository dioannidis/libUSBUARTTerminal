unit usbasp_uart;

{

  This file is part of Object Pascal libUSB UART Terminal for USBasp ( Firmware 1.5 patched ).

  libUSB UART Terminal types and helper functions.

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
  libusb;

const
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
  USBaspUART = record
    USBHandle: plibusb_device_handle;
    USBContext: plibusb_context;
  end;

function usbasp_uart_open(var AUSBasp: USBaspUART): integer;
function usbasp_uart_config(var AUSBasp: USBaspUART; ABaud: integer;
  AFlags: integer): integer;
function usbasp_uart_read(var AUSBasp: USBaspUART; ABuff: PChar; len: integer): integer;
function usbasp_uart_write(var AUSBasp: USBaspUART; ABuff: PChar;
  len: integer): integer;
function usbasp_uart_disable(var AUSBasp: USBaspUART): integer;

implementation

const
  USB_ERROR_NOTFOUND = 1;
  USB_ERROR_ACCESS = 2;
  USB_ERROR_IO = 3;

  USBASP_SHARED_VID = $16C0;
  USBASP_SHARED_PID = $05DC;

  USBASP_FUNC_UART_CONFIG = 60;
  USBASP_FUNC_UART_FLUSHTX = 61;
  USBASP_FUNC_UART_FLUSHRX = 62;
  USBASP_FUNC_UART_DISABLE = 63;
  USBASP_FUNC_UART_TX = 64;
  USBASP_FUNC_UART_RX = 65;
  USBASP_FUNC_UART_TX_FREE = 66;
  USBASP_FUNC_UART_RX_FREE = 67;

  USBASP_FUNC_GETCAPABILITIES = 127;

  USBASP_CAP_6_UART = (1 shl 6);

  USBASP_NO_CAPS = (-4);

type
  pplibusb_device = ^plibusb_device;
  ppplibusb_device = ^pplibusb_device;

var
  locDummy: array [0..3] of byte;

function usbasp_uart_open(var AUSBasp: USBaspUART): integer;

  function CompareDescriptor(AUSBHandle: plibusb_device_handle;
    ADescriptor: uint8_t; AStringValue: string): boolean;
  var
    iStrLength: integer;
    StrBuffer: array [0..255] of byte;
    StrTemp: string;
  begin
    iStrLength := libusb_get_string_descriptor_ascii(AUSBHandle,
      ADescriptor, @StrBuffer[0], Length(StrBuffer));
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
  AUSBasp.USBHandle := nil;
  AUSBasp.USBContext := nil;
  iResult := libusb_init(AUSBasp.USBContext);
  //libusb_set_debug(AUSBasp.USBContext, 10);
  USBDeviceListCount := libusb_get_device_list(AUSBasp.USBContext,
    plibusb_device(USBDeviceList));
  try
    for i := 0 to USBDeviceListCount - 1 do
    begin
      USBDevice := @USBDeviceList[i]^;
      libusb_get_device_descriptor(USBDevice, USBDeviceDescriptor);
      if (USBDeviceDescriptor.idVendor = USBASP_SHARED_VID) and
        (USBDeviceDescriptor.idProduct = USBASP_SHARED_PID) then
      begin
        if libusb_Open(USBDevice, AUSBasp.USBHandle) = 0 then
        begin
          if CompareDescriptor(AUSBasp.USBHandle, USBDeviceDescriptor.iProduct,
            'USBasp') then
          begin
            libusb_close(AUSBasp.USBHandle);
            AUSBasp.USBHandle := nil;
            continue;
          end;
          if CompareDescriptor(AUSBasp.USBHandle, USBDeviceDescriptor.iManufacturer,
            'www.fischl.de') then
          begin
            libusb_close(AUSBasp.USBHandle);
            AUSBasp.USBHandle := nil;
            continue;
          end;
          break;
        end;
      end;
    end;
  finally
    libusb_free_device_list(plibusb_device(USBDeviceList), 1);
    if (libusb_kernel_driver_active(AUSBasp.USBHandle, 0) = 1) then
      //find out if kernel driver is attached
      libusb_detach_kernel_driver(AUSBasp.USBHandle, 0);//detach it
    Result := libusb_claim_interface(AUSBasp.USBHandle, 0);   //claim usb interface
  end;
  //if AUSBasp.USBHandle <> nil then
  //  Result := 0;
end;

function usbasp_uart_transmit(var AUSBasp: USBaspUART; AReceive: uint8_t;
  AFunctionId: uint8_t; ASend: array of byte; ABuffer: PChar;
  ABufferSize: uint16_t): integer;
var
  locResult: integer;
begin
  locResult := libusb_control_transfer(AUSBasp.USBHandle,
    (byte(LIBUSB_REQUEST_TYPE_VENDOR) or byte(LIBUSB_RECIPIENT_DEVICE) or
    (AReceive shl 7)) and $FF,
    //($40 or (AReceive shl 7)) and $FF,
    //AReceive,
    AFunctionId, ((ASend[1] shl 8) or ASend[0]), ((ASend[3] shl 8) or ASend[2]),
    @ABuffer[0], ABufferSize, 5000);
  Result := locResult;
end;

function usbasp_uart_capabilities(var AUSBasp: USBaspUART): uint32_t;
var
  Res: array [0..3] of uint8_t;
  iResult: integer;
begin
  Result := 0;
  iResult := usbasp_uart_transmit(AUSBasp, 1, USBASP_FUNC_GETCAPABILITIES,
    locDummy, PChar(@Res[0]), length(Res));
  if iResult = 4 then
    Result := Res[0] or (uint32_t(Res[1]) shl 8) or (uint32_t(Res[2]) shl 16) or
      (Ord(Res[3]) shl 24);
end;

function usbasp_uart_config(var AUSBasp: USBaspUART; ABaud: integer;
  AFlags: integer): integer;
var
  Caps: uint32_t;
  FOSC: integer = 12000000;
  Presc, RealBaud, iResult: integer;
  Send: array [0..3] of byte;
begin
  if usbasp_uart_open(AUSBasp) <> 0 then
  begin
    Result := -1;
    Exit;
  end;

  Caps := usbasp_uart_capabilities(AUSBasp);
  if (caps and USBASP_CAP_6_UART) = 0 then
  begin
    Result := USBASP_NO_CAPS;
    exit;
  end;

  try
    Presc := FOSC div 8 div ABaud - 1;
    RealBaud := FOSC div 8 div (Presc + 1);
  except
  end;

  if RealBaud <> ABaud then
  begin
    //Result := -1;
    //exit;
  end;

  FillChar(Send, SizeOf(Send), 0);
  Send[1] := Presc shr 8;
  Send[0] := Presc and $FF;
  Send[2] := AFlags and $FF;

  iResult := usbasp_uart_transmit(AUSBasp, 1, USBASP_FUNC_UART_CONFIG,
    Send, PChar(@locDummy[0]), 0);
  Result := 0;
end;

function usbasp_uart_read(var AUSBasp: USBaspUART; ABuff: PChar; len: integer): integer;
begin
  if (len > 254) then
    len := 254;
  // The USBasp V-USB library is not configured with USB_CFG_LONG_TRANSFERS for long transfers.
  Result := usbasp_uart_transmit(AUSBasp, 1, USBASP_FUNC_UART_RX, locDummy, ABuff, len);
end;

function usbasp_uart_write(var AUSBasp: USBaspUART; ABuff: PChar;
  len: integer): integer;
var
  TXFree: array[0..1] of byte;
  TXAvail: integer;
begin
  FillChar(TXFree, SizeOf(TXFree), 0);
  usbasp_uart_transmit(AUSBasp, 1, USBASP_FUNC_UART_TX_FREE, locDummy,
    PChar(@TXFree[0]), 2);
  TXAvail := (TXFree[0] shl 8) or byte(TXFree[1]);
  if TXAvail = 0 then
    exit;
  if len > TXAvail then
    len := TXAvail;
  Result := usbasp_uart_transmit(AUSBasp, 0, USBASP_FUNC_UART_TX, locDummy, ABuff, len);
end;

function usbasp_uart_disable(var AUSBasp: USBaspUART): integer;
var
  iResult: integer;
begin
  iResult := usbasp_uart_transmit(AUSBasp, 1, USBASP_FUNC_UART_DISABLE,
    locDummy, PChar(@locDummy[0]), 0);
  iResult := libusb_release_interface(AUSBasp.USBHandle, 0);
  libusb_close(AUSBasp.USBHandle);
  libusb_exit(AUSBasp.USBContext);
end;

initialization
  FillChar(locDummy, SizeOf(locDummy), 0);

end.
