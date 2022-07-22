unit USBasp_LIBUSB;

{

  This file is part of Nephelae's Object Pascal FPUSBasp.

  LIBUSB Communications.

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
  USBasp_Definitions, libusb;

function usbasp_libusb_devices(var AUSBaspDeviceList: TUSBasp_USBDeviceList): integer;
function usbasp_libusb_initialization: integer;
function usbasp_libusb_finalization: integer;

function usbasp_libusb_open(const AUSBasp: PUSBasp_USBDevice): integer;
function usbasp_libusb_close(const AUSBasp: PUSBasp_USBDevice): integer;

//function usbasp_libusb_uart_enable(const AUSBasp: PUSBaspDevice;
//  var ARealBaud: integer; AFlags: integer): integer;
//function usbasp_libusb_uart_disable(const AUSBasp: PUSBaspDevice): integer;

//function usbasp_libusb_uart_read(const AUSBaspHandle: plibusb_device_handle;
//  ABuff: PChar; len: integer): integer;
//function usbasp_libusb_uart_write(const AUSBaspHandle: plibusb_device_handle;
//  ABuff: PChar; len: integer): integer;

implementation

uses
  SysUtils;

type
  pplibusb_device = ^plibusb_device;
  ppplibusb_device = ^pplibusb_device;

type
  TCapabilities = record
    case boolean of
      True: (a: longword);
      False: (bytes: array [0..3] of byte);
  end;

var
  GlobalContext: plibusb_context;
  //HotPlugCallbackHandle: libusb_hotplug_callback_handle;
  USBDeviceList: ppplibusb_device;
  USBDeviceListCount: integer;
  usbcallresult: integer;
  locDummy: array [0..3] of byte;

function usbasp_libusb_uart_transmit(const AUSBaspHandle: plibusb_device_handle;
  AReceive: uint8_t; AFunctionId: uint8_t; const ASend: array of byte;
  ABuffer: PChar; ABufferSize: uint16_t): integer;
begin
  Result := libusb_control_transfer(AUSBaspHandle,
    (byte(LIBUSB_REQUEST_TYPE_VENDOR) or byte(LIBUSB_RECIPIENT_DEVICE) or
    (AReceive shl 7)) and $FF, AFunctionId, ((ASend[1] shl 8) or ASend[0]),
    ((ASend[3] shl 8) or ASend[2]), @ABuffer[0], ABufferSize, 5000);
end;

function usbasp_libusb_uart_capabilities(
  const AUSBaspHandle: plibusb_device_handle): uint32_t;
var
  Caps: array [0..3] of uint8_t;
  Rslt: int32;
begin
  Rslt := usbasp_libusb_uart_transmit(AUSBaspHandle, 1, USBASP_FUNC_GETCAPABILITIES,
    locDummy, PChar(@Caps[0]), length(Caps));
  if Rslt = 4 then
    Result := Caps[0] or (uint32_t(Caps[1]) shl 8) or (uint32_t(Caps[2]) shl 16) or
      (Ord(Caps[3]) shl 24);
end;

function usbasp_libusb_initialization: integer;
begin
  FillChar(locDummy, SizeOf(locDummy), 0);
  USBDeviceList := nil;
  Result := libusb_init(GlobalContext);
end;

function usbasp_libusb_finalization: integer;
begin
  try
    libusb_free_device_list(plibusb_device(USBDeviceList), USBDeviceListCount);
    libusb_exit(GlobalContext);
  except
    Result := -1;
  end;
  Result := 0;
end;

function usbasp_libusb_open(const AUSBasp: PUSBasp_USBDevice): integer;
begin
  Result := libusb_open(AUSBasp^.Device, AUSBasp^.Handle);
  //find out if kernel driver is attached
  if (libusb_kernel_driver_active(AUSBasp^.Handle, 0) = 1) then
    libusb_detach_kernel_driver(AUSBasp^.Handle, 0);//detach it
  //claim usb interface
  if libusb_claim_interface(AUSBasp^.Handle, 0) = 0 then
    AUSBasp^.Interface0Claimed := True;
end;

function usbasp_libusb_close(const AUSBasp: PUSBasp_USBDevice): integer;
begin
  Result := 0;
  if AUSBasp^.Interface0Claimed then
    Result := libusb_release_interface(AUSBasp^.Handle, 0);
  libusb_close(AUSBasp^.Handle);
  AUSBasp^.Handle := nil;
  AUSBasp^.Interface0Claimed := False;
end;

function usbasp_libusb_devices(var AUSBaspDeviceList: TUSBasp_USBDeviceList): integer;

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
    AUSBDeviceDescriptor: libusb_device_descriptor): PUSBasp_USBDevice;
  var
    tmpUSBaspDevice: PUSBasp_USBDevice;
    tmpUSBBOSDescriptor: plibusb_bos_descriptor;
    tmpUSBBOSContainerIDDescriptor: plibusb_container_id_descriptor;
    //ErrorMsg: PAnsiChar;
    Caps: uint32_t;
    x: integer;
  begin
    //GetMem(tmpUSBaspDevice, SizeOf(TUSBDevice));
    New(tmpUSBaspDevice);

    usbcallresult := libusb_get_bos_descriptor(AUSBaspHandle, tmpUSBBOSDescriptor);
    if usbcallresult = 0 then
    begin
      try
        for x := 0 to tmpUSBBOSDescriptor^.bNumDeviceCaps - 1 do
        begin
          if libusb_bos_type(tmpUSBBOSDescriptor^.dev_capability[x]^.bDevCapabilityType) =
            LIBUSB_BT_CONTAINER_ID then
          begin
            if libusb_get_container_id_descriptor(GlobalContext,
              tmpUSBBOSDescriptor^.dev_capability[x], tmpUSBBOSContainerIDDescriptor) = 0 then
              begin
                try
                  tmpUSBaspDevice^.ContainerID := TGuid.Create(tmpUSBBOSContainerIDDescriptor^.ContainerID);
                finally
                  libusb_free_container_id_descriptor(tmpUSBBOSContainerIDDescriptor);
                end;
                Break;
              end;
          end;
        end;
      finally
        libusb_free_bos_descriptor(tmpUSBBOSDescriptor);
      end;
    end;

    tmpUSBaspDevice^.Device := AUSBDevice;
    tmpUSBaspDevice^.HidDevice := nil;
    tmpUSBaspDevice^.MonitorHidDevice := nil;
    tmpUSBaspDevice^.Handle := nil;
    tmpUSBaspDevice^.ProductName :=
      GetDescriptorString(AUSBaspHandle, AUSBDeviceDescriptor.iProduct);
    tmpUSBaspDevice^.Manufacturer :=
      GetDescriptorString(AUSBaspHandle, AUSBDeviceDescriptor.iManufacturer);
    tmpUSBaspDevice^.SerialNumber :=
      GetDescriptorString(AUSBaspHandle, AUSBDeviceDescriptor.iSerialNumber);

    Caps := usbasp_libusb_uart_capabilities(AUSBaspHandle);
    tmpUSBaspDevice^.HasTPI :=
      (TCapabilities(Caps).bytes[0] and USBASP_CAP_0_TPI) <> 0;
    tmpUSBaspDevice^.HasUart :=
      (TCapabilities(Caps).bytes[0] and USBASP_CAP_6_UART) <> 0;
    tmpUSBaspDevice^.HasHIDUart :=
      (TCapabilities(Caps).bytes[0] and USBASP_CAP_7_HID_UART) <> 0;

    //tmpUSBaspDevice^.CrystalOsc := (TCapabilities(Caps).bytes[1]) * 1000000;
    case TCapabilities(Caps).bytes[1] of
      USBASP_CAP_12MHZ_CLOCK: tmpUSBaspDevice^.CrystalOsc := 12000000;
      USBASP_CAP_16MHZ_CLOCK: tmpUSBaspDevice^.CrystalOsc := 16000000;
      USBASP_CAP_18MHZ_CLOCK: tmpUSBaspDevice^.CrystalOsc := 18000000;
      USBASP_CAP_20MHZ_CLOCK: tmpUSBaspDevice^.CrystalOsc := 20000000;
    end;

    Result := tmpUSBaspDevice;
  end;

var
  USBDevice: plibusb_device;
  USBDeviceDescriptor: libusb_device_descriptor;
  i: integer;
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
    tmpHandle := nil;
    usbcallresult := libusb_get_device_descriptor(USBDevice, USBDeviceDescriptor);
    if (USBDeviceDescriptor.idVendor = USBASP_SHARED_VID) and
      (USBDeviceDescriptor.idProduct = USBASP_SHARED_PID) then
    begin
      if libusb_Open(USBDevice, tmpHandle) = 0 then
      begin
        AUSBaspDeviceList.Add(USBDeviceToUSBaspRecord(USBDevice,
          tmpHandle, USBDeviceDescriptor));
      end;
      libusb_close(tmpHandle);
    end;
  end;
  Result := AUSBaspDeviceList.Count;
end;

//function usbasp_hotplug_callback(AContext: plibusb_context;
//  AHotPlugDevice: plibusb_device; AEvent: libusb_hotplug_event;
//  AUserDat: Pointer): integer;
//begin
//  if AEvent = LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED then
//  ;

//  Result := 0;
//end;

//function usbasp_libusb_uart_enable(const AUSBasp: PUSBasp_USBDevice;
//  var ARealBaud: integer; AFlags: integer): integer;
//var
//  Prescaler: integer;
//  UARTConfig: array [0..3] of byte;
//begin
//  Prescaler := AUSBasp^.FOsc div 8 div ARealBaud - 1;
//  ARealBaud := AUSBasp^.FOsc div 8 div (Prescaler + 1);

//  FillChar(UARTConfig, SizeOf(UARTConfig), 0);
//  UARTConfig[0] := Prescaler and $FF;
//  UARTConfig[1] := Prescaler shr 8;
//  UARTConfig[2] := AFlags and $FF;

//  Result := usbasp_libusb_uart_transmit(AUSBasp^.LIBUSBHandle, 1, USBASP_FUNC_UART_CONFIG,
//    UARTConfig, PChar(@locDummy[0]), 0);
//end;

//function usbasp_libusb_uart_read(const AUSBaspHandle: plibusb_device_handle;
//  ABuff: PChar; len: integer): integer;
//begin
//  if (len > 254) then
//    len := 254;
//  // The USBasp V-USB library is not configured with USB_CFG_LONG_TRANSFERS for long transfers.
//  Result := usbasp_libusb_uart_transmit(AUSBaspHandle, 1, USBASP_FUNC_UART_RX,
//    locDummy, ABuff, len);
//end;

//function usbasp_libusb_uart_write(const AUSBaspHandle: plibusb_device_handle;
//  ABuff: PChar; len: integer): integer;
//var
//  TXFree: array[0..1] of byte;
//  TXAvail: integer;
//begin
//  Result := 0;
//  FillChar(TXFree, SizeOf(TXFree), 0);
//  usbasp_libusb_uart_transmit(AUSBaspHandle, 1, USBASP_FUNC_UART_TX_FREE, locDummy,
//    PChar(@TXFree[0]), 2);
//  TXAvail := (TXFree[0] shl 8) or byte(TXFree[1]);
//  if TXAvail = 0 then
//    exit;
//  if len > TXAvail then
//    len := TXAvail;
//  Result := usbasp_libusb_uart_transmit(AUSBaspHandle, 0, USBASP_FUNC_UART_TX,
//    locDummy, ABuff, len);
//end;

//function usbasp_libusb_uart_disable(const AUSBasp: PUSBasp_USBDevice): integer;
//begin
//  Result := usbasp_libusb_uart_transmit(AUSBasp^.LIBUSBHandle, 1, USBASP_FUNC_UART_DISABLE,
//    locDummy, PChar(@locDummy[0]), 0);
//end;

//initialization
//  locResult := libusb_hotplug_register_callback(
//    GlobalContext, libusb_hotplug_event(byte(LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED) or
//    byte(LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT)), LIBUSB_HOTPLUG_NO_FLAGS,
//    USBASP_SHARED_VID, USBASP_SHARED_PID, LIBUSB_HOTPLUG_MATCH_ANY,
//    @usbasp_hotplug_callback, nil, HotPlugCallbackHandle)

end.
