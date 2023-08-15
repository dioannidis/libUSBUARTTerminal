{ TPoweredby Component

  Copyright (C)2014 Gordon Bamber minesadorada@charcodelvalle.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit uPoweredby;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, Forms, Graphics, LResources, SysUtils,
  ExtCtrls, InterfaceBase, LCLType,LCLVersion,AboutPoweredbyunit;

const
  C_VERSIONSTRING = '1.0.4.0';
  C_WIDGETSET_GTK = 'GTK widget set';
  C_WIDGETSET_GTK2 = 'GTK 2 widget set';
  C_WIDGETSET_GTK3 = 'GTK 3 widget set';
  C_WIDGETSET_WIN = 'Win32/Win64 widget set';
  C_WIDGETSET_WINCE = 'WinCE widget set';
  C_WIDGETSET_CARBON = 'Carbon widget set';
  C_WIDGETSET_QT = 'QT widget set';
  C_WIDGETSET_fpGUI = 'fpGUI widget set';
  C_WIDGETSET_COCOA = 'Cocoa widget set';
  C_WIDGETSET_CUSTOM = 'Custom drawn widget set';
  C_WIDGETSET_OTHER = 'Other gui';

type
  TPoweredby = class(TAboutPoweredBy)
  private
    { Private declarations }
    fPoweredByForm: TForm;
    fVersionString: string;
    fDelayMilliseconds: integer;
    fFadeInMilliseconds: integer;
    fShowOnlyOnce,fAlreadyShown:Boolean;
    // Used by Timer to close the PoweredBy form
    procedure ClosePoweredByForm(Sender: TObject);
    // Windows only!
    procedure FadeInPoweredBy(Sender: TObject);
    procedure SetDelayMilliSeconds(AValue: integer);
    function GetWidgetSetString: string;
    Function GetFPCTargetInfoString: String;
    Function GetInfoLCLVersion:String;
    Function GetInfoFPCVersion:String;
  protected
    { Protected declarations }
  public
    { Public declarations }
    // Call the method 'ShowPoweredByForm' to show the shaped window
    procedure ShowPoweredByForm;
    // Called when component is dropped onto a form
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    // Minimum delay=1000msec; Maximum delay=10000msec.  Fade-in time is automatically adjusted
    property DelayMilliSecs: integer read fDelayMilliSeconds write SetDelayMilliSeconds default 1000;
    // Call the method 'ShowPoweredByForm' to show the shaped window
    property Version: string read fVersionString;
    // Reports the current WidgetSet
    property InfoWidgetSet: string read GetWidgetSetString;
    // Reports your current Environment
    property InfoFPCTarget:String read GetFPCTargetInfoString;
    // Reports your current Environment
    property InfoFPCVersion:String read GetInfoFPCVersion;
    // Reports your current Environment
    property InfoLCLVersion:String read GetInfoLCLVersion;
    // Useful if you have ShowPoweredByForm in your TForm.Activate() method
    property ShowOnlyOnce:boolean read fShowOnlyOnce write fShowOnlyOnce default false;
  end;

procedure Register;

implementation
{$IF lcl_fullversion > 200600}Uses LCLPlatformDef;{$ENDIF}

procedure Register;
begin
  {$I upoweredby_icon.lrs}
  RegisterComponents('Additional', [TPoweredby]);
end;

constructor TPoweredby.Create(AOwner: TComponent);
  // Initialise private vars
begin
  inherited Create(AOwner);
  fVersionString := C_VERSIONSTRING;
  fDelayMilliseconds := 1000;
  fFadeInMilliseconds := 20;
  fAlreadyShown:=False;
  fShowOnlyOnce:=False;
  // About dialog
  AboutBoxComponentName:='PoweredBy component';
  AboutBoxWidth:=400;
//  AboutBoxHeight (integer)
  AboutBoxDescription:='Component that shows a Powered By graphic.' + LineEnding +
  'Use method ShowPoweredByForm in your form.create()' + LineEnding +
  'to use the component';
  AboutBoxBackgroundColor:=clWindow;
  AboutBoxFontName:='Arial';
  AboutBoxFontSize:=10;
  AboutBoxVersion:=C_VERSIONSTRING;
  AboutBoxAuthorname:='Gordon Bamber';
  AboutBoxOrganisation:='Public Domain';
  AboutBoxAuthorEmail:='minesadorada@charcodelvalle.com';
  AboutBoxLicenseType:='MODIFIEDGPL';
end;
Function TPoweredby.GetInfoLCLVersion:String;
begin
  result:=lcl_version;
end;
Function TPoweredby.GetInfoFPCVersion:String;
begin
  Result:={$I %FPCVERSION%};
end;
Function TPoweredby.GetFPCTargetInfoString: String;
begin
  Result := {$I %FPCTARGETCPU%}+' - '+{$I %FPCTARGETOS%};
end;

function priv_GetWidgetSetString: string;
  // This code cannot be a method of TPoweredBy
begin
  case WidgetSet.LCLPlatform of
    lpGtk: Result := C_WIDGETSET_GTK;
    lpGtk2: Result := C_WIDGETSET_GTK2;
    lpWin32: Result := C_WIDGETSET_WIN;
    lpWinCE: Result := C_WIDGETSET_WINCE;
    lpCarbon: Result := C_WIDGETSET_CARBON;
    lpCocoa: Result := C_WIDGETSET_COCOA;
    lpQT: Result := C_WIDGETSET_QT;
    lpfpGUI: Result := C_WIDGETSET_fpGUI;
    // When were these first included in InterfaceBase?
    {$IFDEF FPC_FULLVERSION>24200}
    lpGtk3: Result := C_WIDGETSET_GTK3;
    lpCustomDrawn: Result := C_WIDGETSET_CUSTOM;
    {$ENDIF}
    else
      Result := C_WIDGETSET_OTHER;
  end;
end;

function TPoweredby.GetWidgetSetString: string;
begin
  Result := priv_GetWidgetSetString;
end;

procedure TPoweredby.SetDelayMilliSeconds(AValue: integer);
begin
  if ((fDelayMilliSeconds <> AValue) and (AValue > 0) and (AValue < 11000)) then
  begin
    fDelayMilliseconds := AValue;
    fFadeInMilliseconds := (AValue div 1000) * 20;
  end;

end;

procedure TPoweredby.ClosePoweredByForm(Sender: TObject);
// Called by Timer event in ShowPoweredByForm to close Modal window
// Also the image OnClick event
begin
  fPoweredByForm.Close;
end;

procedure TPoweredby.FadeInPoweredBy(Sender: TObject);
// Use Alphablend property of TForm
begin
  if (fPoweredByForm.AlphaBlendValue < 245) then
    fPoweredByForm.AlphaBlendValue := fPoweredByForm.AlphaBlendValue + 10;
end;

function CanShowRoundedGraphic: boolean;
{
Check the current WidgetSet, and add to the list that can show the rounded graphic
Choices are:
  lpGtk,
  lpGtk2,
  lpGtk3,
  lpWin32,
  lpWinCE,
  lpCarbon,
  lpQT,
  lpfpGUI,
  lpNoGUI,
  lpCocoa,
  lpCustomDrawn
  }
begin
Result:=FALSE;
{
  case WidgetSet.LCLPlatform of
    lpWin32, lpQT: Result := True;
    else
      Result := False;
  end;
}
end;

procedure TPoweredby.ShowPoweredByForm;

// Graphics are in masks.lrs
// 1 ) Constructs a new TForm with an image control
// 2 ) Uses the 'SetShape' method of the form canvas to create a transparent mask
// 3 ) Paints the Timage over it with a color image
// 4 ) Sets a timer to fade it in using the Alphablend property
// 5 ) Sets another timer to close the form

// Note: Windows can fade in a shaped transparent screen
// But some widgetsets (GTK,Carbon) cannot
var
  img_Background: TImage;
  MyBitmap: TBitMap;
  DelayTimer: TTimer;
  FadeInTimer: TTImer;
begin
  // Respect the ShowOnlyOnce property setting
  If ((fShowOnlyOnce=TRUE) AND (fAlreadyShown=TRUE)) then Exit;

  // Try..Finally so we can be sure resources are Freed
  try
    try
      // Create controls
      fPoweredByForm := TForm.Create(nil);
      fPoweredByForm.AlphaBlend := True;
      fPoweredByForm.AlphaBlendValue := 0;
      img_background := TImage.Create(fPoweredByForm);
      // Bitmap mask - Load from resource
      MyBitmap := TBitMap.Create;
      MyBitmap.Monochrome := True;
      MyBitmap.LoadFromLazarusResource('powered_by_mask');
      MyBitMap.Transparent := True;
      MyBitMap.TransparentColor := clBlack;
      // Delay Timer
      Delaytimer := TTimer.Create(fPoweredByForm);
      delaytimer.Interval := fDelayMilliseconds;
      delaytimer.OnTimer := @ClosePoweredByForm;

      FadeInTimer := TTimer.Create(fPoweredByForm);
      FadeInTimer.Interval := fFadeInMilliseconds;
      FadeInTimer.OnTimer := @FadeInPoweredBy;

      // BackGround image - load from resource
      with img_background do
      begin
        Align := alClient;
        Stretch := True;
        Parent := fPoweredByForm;
        if CanShowRoundedGraphic then
          Picture.LoadFromLazarusResource('win_powered_by')
        else
          Picture.LoadFromLazarusResource('powered_by');
        OnClick := @ClosePoweredByForm;
        SendToBack;
      end;
      // Set form properties
      with fPoweredByForm do
      begin
        position := poScreenCenter;
        borderstyle := bsnone;
        formstyle := fsSystemStayOnTop;
        OnClick := @ClosePoweredByForm;
        color := clBlack;
        Height := MyBitmap.Height;
        Width := MyBitMap.Width;
        if CanShowRoundedGraphic then
        begin
          MyBitMap.Transparent := True;
          MyBitMap.TransparentColor := clBlack;
          Canvas.Draw(0, 0, MyBitMap);
          // raises Floating Point Error in linux GTK (!??)
          SetShape(MyBitMap);
        end
        else
        begin
          // If square graphic, then adjust form size
             height:=img_background.Picture.Height;
             width:=img_background.picture.width;
        end;
        // Now show the completed form
        delaytimer.Enabled := True;
        FadeInTimer.Enabled := True;
        Application.ProcessMessages;
        ShowModal; // Closed via the Timer event or a user click
        fAlreadyShown:=TRUE;
      end;
    except
      On E: Exception do
        raise Exception.CreateFmt('%s Error: %s', [Name, Exception.ClassName]);
    end;
  finally
    FreeAndNil(img_background);
    FreeAndNil(MyBitMap);
    FreeAndNil(delayTimer);
    FreeAndNil(FadeInTimer);
    FreeAndNil(fPoweredByForm);
  end;
end;

initialization
  // Load graphics as lazarus resources into the component
{$I graphics.lrs}

end.
