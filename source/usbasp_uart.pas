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
  Classes, libusb;

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

  { TUSBaspDevice }

  TUSBaspDevice = record
    USBDevice: plibusb_device;
    Handle: plibusb_device_handle;
    ProductName: string[255];
    Manufacturer: string[255];
    SerialNumber: string[255];
    HasUart: boolean;
    HasTPI: boolean;
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

function usbasp_devices(var AUSBaspDeviceList: TUSBaspDeviceList): integer;
function usbasp_initialization: integer;
function usbasp_finalization: integer;
function usbasp_open(const AUSBasp: PUSBaspDevice): integer;
function usbasp_close(const AUSBasp: PUSBaspDevice): integer;
function usbasp_uart_enable(const AUSBasp: PUSBaspDevice; ABaud: integer;
  AFlags: integer): integer;
function usbasp_uart_disable(const AUSBasp: PUSBaspDevice): integer;

function usbasp_uart_read(AUSBasp: PUSBaspDevice; ABuff: PChar; len: integer): integer;
function usbasp_uart_write(const AUSBasp: PUSBaspDevice; ABuff: PChar;
  len: integer): integer;

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

  USBASP_CAP_0_TPI = 1;
  USBASP_CAP_6_UART = (1 shl 6);

  USBASP_NO_CAPS = (-4);

type
  pplibusb_device = ^plibusb_device;
  ppplibusb_device = ^pplibusb_device;

var
  GlobalContext: plibusb_context;
  HotPlugCallbackHandle: libusb_hotplug_callback_handle;
  USBDeviceList: ppplibusb_device;
  USBDeviceListCount: integer;
  locResult: integer;
  locDummy: array [0..3] of byte;

function usbasp_uart_transmit(const AUSBaspHandle: plibusb_device_handle;
  AReceive: uint8_t; AFunctionId: uint8_t; ASend: array of byte;
  ABuffer: PChar; ABufferSize: uint16_t): integer;
begin
  Result := libusb_control_transfer(AUSBaspHandle,
    (byte(LIBUSB_REQUEST_TYPE_VENDOR) or byte(LIBUSB_RECIPIENT_DEVICE) or
    (AReceive shl 7)) and $FF, AFunctionId, ((ASend[1] shl 8) or ASend[0]),
    ((ASend[3] shl 8) or ASend[2]), @ABuffer[0], ABufferSize, 5000);
end;

function usbasp_uart_capabilities(const AUSBaspHandle: plibusb_device_handle): uint32_t;
var
  Res: array [0..3] of uint8_t;
  iResult: integer;
begin
  Result := 0;
  iResult := usbasp_uart_transmit(AUSBaspHandle, 1, USBASP_FUNC_GETCAPABILITIES,
    locDummy, PChar(@Res[0]), length(Res));
  if iResult = 4 then
    Result := Res[0] or (uint32_t(Res[1]) shl 8) or (uint32_t(Res[2]) shl 16) or
      (Ord(Res[3]) shl 24);
end;

function usbasp_initialization: integer;
begin
  FillChar(locDummy, SizeOf(locDummy), 0);
  libusb_init(GlobalContext);
  USBDeviceList := nil;
end;

function usbasp_finalization: integer;
begin
  libusb_free_device_list(plibusb_device(USBDeviceList), USBDeviceListCount);
  libusb_exit(GlobalContext);
end;

function usbasp_open(const AUSBasp: PUSBaspDevice): integer;
begin
  Result := libusb_open(AUSBasp^.USBDevice, AUSBasp^.Handle);
  //find out if kernel driver is attached
  if (libusb_kernel_driver_active(AUSBasp^.Handle, 0) = 1) then
    libusb_detach_kernel_driver(AUSBasp^.Handle, 0);//detach it
  //claim usb interface
  if libusb_claim_interface(AUSBasp^.Handle, 0) = 0 then
    AUSBasp^.Interface0Claimed := True;
end;

function usbasp_close(const AUSBasp: PUSBaspDevice): integer;
begin
  Result := 0;
  if AUSBasp^.Interface0Claimed then
    Result := libusb_release_interface(AUSBasp^.Handle, 0);
  libusb_close(AUSBasp^.Handle);
  AUSBasp^.Handle := nil;
  AUSBasp^.HasUart := False;
  AUSBasp^.Interface0Claimed := False;
end;

function usbasp_devices(var AUSBaspDeviceList: TUSBaspDeviceList): integer;

  function GetDescriptorString(AUSBHandle: plibusb_device_handle;
    ADescriptor: uint8_t): string;
  var
    iStrLength: integer;
    StrBuffer: array [0..255] of byte;
    StrTemp: string;
  begin
    iStrLength := libusb_get_string_descriptor_ascii(AUSBHandle,
      ADescriptor, @StrBuffer[0], Length(StrBuffer));
    SetString(StrTemp, PChar(@StrBuffer[0]), iStrLength);
    Result := StrTemp;
  end;

  function USBDeviceToUSBaspRecord(AUSBDevice: plibusb_device;
    AUSBaspHandle: plibusb_device_handle;
    AUSBDeviceDescriptor: libusb_device_descriptor): PUSBaspDevice;
  var
    tmpUSBaspDevice: PUSBaspDevice;
    Caps: uint32_t;
  begin
    GetMem(tmpUSBaspDevice, SizeOf(TUSBaspDevice));
    tmpUSBaspDevice^.USBDevice := AUSBDevice;
    tmpUSBaspDevice^.Handle := nil;
    tmpUSBaspDevice^.ProductName :=
      GetDescriptorString(AUSBaspHandle, AUSBDeviceDescriptor.iProduct);
    tmpUSBaspDevice^.Manufacturer :=
      GetDescriptorString(AUSBaspHandle, AUSBDeviceDescriptor.iManufacturer);
    tmpUSBaspDevice^.SerialNumber :=
      GetDescriptorString(AUSBaspHandle, AUSBDeviceDescriptor.iSerialNumber);

    Caps := usbasp_uart_capabilities(AUSBaspHandle);
    tmpUSBaspDevice^.HasUart := (Caps and USBASP_CAP_6_UART) <> 0;
    tmpUSBaspDevice^.HasTPI := (Caps and USBASP_CAP_0_TPI) <> 0;

    Result := tmpUSBaspDevice;
  end;

var
  USBDevice: plibusb_device;
  USBDeviceDescriptor: libusb_device_descriptor;
  iResult, i, USBaspCount: integer;
  tmpHandle: plibusb_device_handle;
begin
  AUSBaspDeviceList.FreeItems;
  AUSBaspDeviceList.Clear;
  if Assigned(USBDeviceList) then
    libusb_free_device_list(plibusb_device(USBDeviceList), USBDeviceListCount);
  USBDeviceListCount := libusb_get_device_list(GlobalContext,
    plibusb_device(USBDeviceList));
  for i := 0 to USBDeviceListCount - 1 do
  begin
    USBDevice := @USBDeviceList[i]^;
    libusb_get_device_descriptor(USBDevice, USBDeviceDescriptor);
    if (USBDeviceDescriptor.idVendor = USBASP_SHARED_VID) and
      (USBDeviceDescriptor.idProduct = USBASP_SHARED_PID) then
    begin
      if libusb_Open(USBDevice, tmpHandle) = 0 then
        AUSBaspDeviceList.Add(USBDeviceToUSBaspRecord(USBDevice,
          tmpHandle, USBDeviceDescriptor));
      libusb_close(tmpHandle);
    end;
  end;

  Result := AUSBaspDeviceList.Count;
end;

function usbasp_hotplug_callback(AContext: plibusb_context;
  AHotPlugDevice: plibusb_device; AEvent: libusb_hotplug_event;
  AUserDat: Pointer): integer;
begin
  if AEvent = LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED then
  ;

  Result := 0;
end;

function usbasp_uart_enable(const AUSBasp: PUSBaspDevice; ABaud: integer;
  AFlags: integer): integer;
var
  //Caps: uint32_t;
  FOSC: integer = 12000000;
  Presc, RealBaud, iResult: integer;
  Send: array [0..3] of byte;
begin
  //if usbasp_open(AUSBasp) <> 0 then
  //begin
  //  Result := -1;
  //  Exit;
  //end;

  //Caps := usbasp_uart_capabilities(AUSBasp);
  //if (caps and USBASP_CAP_6_UART) = 0 then
  //begin
  //  Result := USBASP_NO_CAPS;
  //  exit;
  //end;

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

  iResult := usbasp_uart_transmit(AUSBasp^.Handle, 1, USBASP_FUNC_UART_CONFIG,
    Send, PChar(@locDummy[0]), 0);
  Result := 0;
end;

function usbasp_uart_read(AUSBasp: PUSBaspDevice; ABuff: PChar; len: integer): integer;
begin
  if (len > 254) then
    len := 254;
  // The USBasp V-USB library is not configured with USB_CFG_LONG_TRANSFERS for long transfers.
  Result := usbasp_uart_transmit(AUSBasp^.Handle, 1, USBASP_FUNC_UART_RX,
    locDummy, ABuff, len);
end;

function usbasp_uart_write(const AUSBasp: PUSBaspDevice; ABuff: PChar;
  len: integer): integer;
var
  TXFree: array[0..1] of byte;
  TXAvail: integer;
begin
  FillChar(TXFree, SizeOf(TXFree), 0);
  usbasp_uart_transmit(AUSBasp^.Handle, 1, USBASP_FUNC_UART_TX_FREE, locDummy,
    PChar(@TXFree[0]), 2);
  TXAvail := (TXFree[0] shl 8) or byte(TXFree[1]);
  if TXAvail = 0 then
    exit;
  if len > TXAvail then
    len := TXAvail;
  Result := usbasp_uart_transmit(AUSBasp^.Handle, 0, USBASP_FUNC_UART_TX,
    locDummy, ABuff, len);
end;

function usbasp_uart_disable(const AUSBasp: PUSBaspDevice): integer;
var
  iResult: integer;
begin
  iResult := usbasp_uart_transmit(AUSBasp^.Handle, 1, USBASP_FUNC_UART_DISABLE,
    locDummy, PChar(@locDummy[0]), 0);
end;

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

initialization
  //locResult := libusb_hotplug_register_callback(
  //  GlobalContext, libusb_hotplug_event(byte(LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED) or
  //  byte(LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT)), LIBUSB_HOTPLUG_NO_FLAGS,
  //  USBASP_SHARED_VID, USBASP_SHARED_PID, LIBUSB_HOTPLUG_MATCH_ANY,
  //  @usbasp_hotplug_callback, nil, HotPlugCallbackHandle)


end.
