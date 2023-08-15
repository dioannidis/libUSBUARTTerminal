unit usplashabout;

{$mode objfpc}{$H+}

interface

{
Credits
=======
uversion.pas by Mike Thompson - mike.cornflake@gmail.com
originally as VersionSupport.pas
See uversion.pas header for more details

========================================================
SplashAbout component by minesadorada@charcodelvalle.com
========================================================
Licence: Modified GPL licence.  Free to use and/or incorporate in your code.

Purpose
=======
Constructs a Splash screen and/or About form with as little effort and resources as possible :)
The windows and controls are created and destroyed on-the-fly to reduce application size
For use in any Lazarus 1.x/FPC 2.x application

Files needed to install component
=================================================
uspalshabout.lpk usplashabout.pas,uversion.pas

Optional Files (in the same folder as your application)
=======================================================
Optional: gpl.txt, lgpl.txt,modifiedgpl.txt,mit.txt
Optional: <splashgraphic>.(bmp/jpg/gif)
Optional: <splashicon>.ico
Optional: <maskgraphic>.bmp

How to use in your project (see TestApp project)
================================================
Use the Property Editor to set propertes as normal
or override them as below before calling ShowSplash or ShowAbout

// If SplashAbout1.DelaySeconds set to zero, then Splash will close only when your application becomes idle
SplashAbout1.UserTitle:='My Superb App'; // Will override property only of TitleStyle is set to saUserTitle
SplashAbout1.OptionalIconFilePath:='myicon.ico'; // OPTIONAL.  If TitleStyle is saUserTitle then overrides default
SplashAbout1.BackGroundImageFilePath:='splash.jpg'; // OPTIONAL.  Default is no Background Image. Optimal size=320 x 240
SplashAbout1.MaskImageFilePath:='roundedrect.bmp'; // OPTIONAL.
// Makes a shaped splash form. Optimum source: .BMP image, 2-bit color depth or more, black area=invisible, white area=visible. Size >= 320 x 240
// If a jpg file is specified by mistake, it will be converted to a (large filesize 24bbp!) .bmp and saved to disk on first run.
SplashAbout1.LicenseFile ; // Default is for Licence button to be absent on ShowAbout method
SplashAbout1.CreditText:='Freeware by minesadorada'; // Default is no text
SplashAbout1.Author:='Mines A. Dorada'; // OPTIONAL.  Default is boilerplate text in LicenseFile Path
SplashAbout1.SupportContact:='minesadorada@charcodelvalle.com'; // OPTIONAL.  Default is boilerplate text in LicenseFile Path

SplashAbout1.ShowSplash;
=============================

EXAMPLE USE in Help/About
=========================
SplashAbout1.ShowAbout;
}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Buttons,
  LCLIntf, LCLType, LResources,
  ExtCtrls, StdCtrls, StrUtils, Dialogs, uversion, uPoweredby, PropEdits;

type
  tFormType = (fSplash, fAbout);
  tLicenseFile = (saNone, saGPL, saLGPL, saMIT, saModifiedGPL, saUserFile);
  tCreateType = (saUserTitle, saForm, saApplication);
  tmasktype = (saNomask, saRoundedRect, saBigCog, saBigFlower, saBigSplash, saUserImage);

const
  C_DEFAULTSPLASHWIDTH = 320;
  C_DEFAULTSPLASHHEIGHT = 240;
  C_DEFAULTSPLASHHEIGHT_LINUX = 280;

  C_DEFAULTLICENSEFORMWIDTH = 500;
  C_DEFAULTLICENSEFORMWIDTH_LINUX = 600;
  C_DEFAULTLICENSEFORMHEIGHT = 400;
  C_DEFAULTLICENSEFORMHEIGHT_LINUX = 450;


type
  TSplashAbout = class(TComponent)
  private
    fSplashForm: TForm;
    fFormType: tFormType;
    fIcon: TIcon;
    fDelaySeconds: integer;
    fTitleString: string;
    fBackGroundColor: TColor;
    fIconFilePath, fBackGroundImageFilePath, fMaskImageFilePath: string;
    fVersionInfoString: string;
    fAppVersionString: string;
    fLicenseFile: tLicenseFile;
    fLicenseTypeString: string;
    fCreditString: string;
    fAuthorString: string;
    fSupportContactString: string;
    fCloseOnIdle: boolean;
    szLicenseFile: string;
    fCreateType: tCreateType;
    fShowDescription: boolean;
    fDescription: string;
    // Initialised in Constructor
    fFormTitleString: string;
    fFormIcon: TIcon;
    fMaskType: tMaskType;
    fPoweredBy: TPoweredBy;
    procedure CloseForm(Sender: TObject);
    procedure ShowForm;
    procedure ShowLicense(Sender: TObject);
    function MakeBMPfromJPG(var JPGFilePath: string): boolean;
    procedure ApplicationOnIdle(Sender: TObject; var {%H-}Done: boolean);
  protected
  public
    // Use anything from here..
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowSplash;
    procedure ShowAbout;
  published
    property PoweredBy: TPoweredBy read fPoweredBy write fPoweredBy;
    property DelaySeconds: integer read fDelaySeconds write fDelaySeconds default 2;
    property BackGroundColor: TColor read fBackGroundColor
      write fBackGroundColor default clSkyBlue;
    property OptionalIconFilePath: string read fIconFilePath write fIconFilePath;
    property BackGroundImageFilePath: string
      read fBackGroundImageFilePath write fBackGroundImageFilePath;
    property MaskImageFilePath: string read fMaskImageFilePath write fMaskImageFilePath;
    property MaskType: tMaskType read fMaskType write fMaskType default saNomask;

    property LicenseFile: tLicenseFile
      read fLicenseFile write fLicenseFile default saNone;
    // property LicenseName: string read fLicenseTypeString write fLicenseTypeString;
    property CreditText: string read fCreditString write fCreditString;
    property SupportContact: string read fSupportContactString
      write fSupportContactString;
    property Author: string read fAuthorString write fAuthorString;
    property TitleStyle: TCreateType read fCreateType write fCreateType default
      saUserTitle;
    property UserTitle: string read fTitleString write fTitleString;
    property ShowDescription: boolean read fShowDescription write fShowDescription;
    property Description: string read fDescription write fDescription;
  end;

procedure Register;


implementation

procedure Register;
begin
  {$I splashabout_ico.lrs}
  RegisterComponents('Additional', [TSplashAbout]);
  RegisterPropertyEditor(TypeInfo(TPoweredBy),
    TSplashAbout, 'PoweredBy', TClassPropertyEditor);
end;

destructor TSplashAbout.Destroy;
begin
  Application.RemoveOnIdleHandler(@ApplicationOnIdle);
  inherited Destroy;
end;

constructor TSplashAbout.Create(AOwner: TComponent);
  // Initialise private vars
var
  Frm: TForm;
begin
  inherited Create(AOwner);

  // Use tPoweredBy as a subcomponent
  // Register a TClassPropertyEditor in order to display it correctly
  fPoweredBy := TPoweredBy.Create(Self);
  fPoweredBy.SetSubComponent(True);  // Tell the IDE to store the modified properties
  fPoweredBy.Name := 'PoweredBy';
  // From main form [Create(Self)]
  if (AOwner is TForm) then
  begin
    Frm := AOwner as TForm;
    fFormTitleString := Frm.Caption;
    fFormIcon := Frm.Icon;
  end;
  DelaySeconds := 2;
  BackGroundColor := clSkyBlue;
  fAppVersionString := LineEnding;
  // Use uversion unit public methods to populate (Credit: Mike Thompson)
  if (GetFileVersion <> 'No build information available') then
    fAppVersionString += 'Version ' + GetFileVersion + LineEnding + LineEnding;
  fVersionInfoString := fAppVersionString + 'Made with: ' + GetLCLVersion;
  fVersionInfoString += ' and ' + GetCompilerInfo + LineEnding;
  fVersionInfoString += 'For: ' + GetTargetInfo + ' (' + GetWidgetSet + ')' + LineEnding;
  fVersionInfoString += 'Last Compiled: ' + GetCompiledDate;

  // Optional property values
  fBackGroundImageFilePath := '';
  fMaskImageFilePath := '';
  fTitleString := 'My Application';
  fCloseOnIdle := False;
end;

procedure TSplashAbout.ApplicationOnIdle(Sender: TObject; var Done: boolean);
begin
  if fSplashForm = nil then
    exit;
  if fCloseOnIdle then
    if fSplashForm.Visible then
    begin
      fSplashForm.ModalResult := mrClose;
      Done := True;
    end;
end;

function TSplashAbout.MakeBMPfromJPG(var JPGFilePath: string): boolean;
  // The MaskImageFilePath has to be a BMP image file (ideally 2bbp).
  // If the developer sets the property to a JPG image file, then this routine
  // will convert it to BMP and save it to disk.  The resulting BMP file is likely
  // to be a huge 24bbp file!
  // WARNING! The routine overwrites any existing BMP with the name name as the old JPG
var
  JPG: TJpegImage;
  BMP: TBitmap;
  s: string;
begin
  Result := False;
  if not FileExists(JPGFilePath) then
    exit;
  try
    JPG := TJpegImage.Create;
    BMP := TBitmap.Create;
    try
      JPG.LoadFromFile(JPGFilePath);
      BMP.Assign(jpg);
      s := ChangeFileExt(JPGFilePath, '.bmp');
      if FileExists(s) then
        ShowMessageFmt('Over-writing old %s', [s]);
      BMP.SaveToFile(s);
      if FileExists(s) then
      begin
        JPGFilePath := s;
        Result := True;
      end;
    except
      On E: Exception do
        MessageDlg('SplashAbout Error',
          'There is something wrong with the MaskImage JPG File' +
          LineEnding + 'Error detail: ' + e.message,
          mtError, [mbOK], 0);
    end;
  finally
    BMP.Free;
    JPG.Free;
  end;
end;

procedure TSplashAbout.ShowLicense(Sender: TObject);
// Triggered by License button Click
var
  sLicenseString: string;
  theList: TStringList;
  f: integer;
  LicenceForm: TForm;
  lblText: TLabel;
  closebuttton: TBitBtn;
  r: TLResource;
begin
  // Trap for invalid path
  case fLicenseFile of
    saNone: szLicenseFile := '';
    saGPL: szLicenseFile := 'gpl.txt';
    saLGPL: szLicenseFile := 'lgpl.txt';
    saMIT: szLicenseFile := 'mit.txt';
    saModifiedgpl: szLicenseFile := 'modifiedgpl.txt';
  end;
  if fLicenseFile = saNone then
    Exit;
  //  if not FileExists(szLicenseFile) then exit;

  // Use a string list to split the text file into lines
  theList := TStringList.Create;
  // Create a window, label and close button on-the-fly
  LicenceForm := TForm.Create(nil);
  lblText := TLabel.Create(LicenceForm);
  closebuttton := TBitBtn.Create(LicenceForm);
  // Load up the text into variable 'sLicenseString'
  sLicenseString := LineEnding + LineEnding + fTitleString + LineEnding;
  try
    try
      // theList.LoadFromFile(szLicenseFile);
      r := LazarusResources.Find(szLicenseFile);
      if r = nil then
        raise Exception.Create('Resource datafile license.lrs is missing');
      thelist.Add(r.Value);
      for f := 0 to TheList.Count - 1 do
        sLicenseString += TheList[f] + LineEnding;
    except
      On e: Exception do
        MessageDlg('SplashAbout Error',
          'There is something wrong with the Licence text', mtError, [mbOK], 0);
    end;

    // Replace boilerplate text if possible
    sLicenseString := AnsiReplaceText(sLicenseString, '<year>',
{$I %DATE%}
      );
    sLicenseString := AnsiReplaceText(sLicenseString, '<name of author>', fAuthorString);
    sLicenseString := AnsiReplaceText(sLicenseString, '<contact>',
      '(' + fSupportContactString + ')');
    sLicenseString := AnsiReplaceText(sLicenseString, '<copyright holders>',
      fAuthorString);

    // Make up the form window and controls
    with LicenceForm do
    begin
      // Form
      {$IFDEF WINDOWS}
      // More compact GUI?
      Width := C_DEFAULTLICENSEFORMWIDTH;
      Height := C_DEFAULTLICENSEFORMHEIGHT;
      {$ELSE WINDOWS}
      Width := C_DEFAULTLICENSEFORMWIDTH_LINUX;
      Height := C_DEFAULTLICENSEFORMHEIGHT_LINUX;
      {$ENDIF}
      // autosize:=true;
      // If you enable autosize, the button placement goes awry!

      // The Modified GPL has an extra clause
      if (szLicenseFile = 'modifiedgpl.txt') or
        (Pos('As a special exception', sLicenseString) > 0) then
        Height := Height + 100;
      position := poScreenCenter;
      borderstyle := bsToolWindow;
      Caption := fTitleString + ' Licensing';
      formstyle := fsSystemStayOnTop;

      // Label
      lblText.Align := alClient;
      lblText.Alignment := taCenter;
      lblText.Caption := sLicenseString;
      lblText.Parent := LicenceForm;

      // Close Button
      closebuttton.Kind := bkClose;
      closebuttton.left := (Width div 2) - closebuttton.Width div 2;
      closebuttton.top := Height - closebuttton.Height - 10;
      closebuttton.parent := LicenceForm;
      // Show modally over the existing modal form
      ShowModal;
    end;
  finally
    // Free up all created resources from memory
    FreeAndNil(theList);
    FreeAndNil(lblText);
    FreeAndNil(closebuttton);
    FreeAndNil(LicenceForm);
  end;
end;

procedure TSplashAbout.CloseForm(Sender: TObject);
// Triggered by a Timer.OnTimer event or CloseButton.Click or OnClick
begin
  fSplashForm.Close; // Hide and destroy
end;

procedure TSplashAbout.ShowSplash;
begin
  // Set the mode, then create and show the form
  fFormType := fSplash;
  ShowForm;
end;

procedure TSplashAbout.ShowAbout;
begin
  // Set the mode, then create and show the form
  fFormType := fAbout;
  ShowForm;
end;

procedure TSplashAbout.ShowForm;
// Main method
// Construct a form and show it modally
// Controls vary according to fFormType variable
var
  okbutton, LicenseButton: TBitBtn;
  Delaytimer, scrolltimer: TTimer;
  lbl_Title, lbl_VersionInfo: TLabel;
  img_icon, img_background: TImage;
  bevel: TBevel;
  MyBitMap: TBitMap;
  sVersionInfoString: string;
  iFormHeight, iFormWidth: integer;
  szMaskName: string;

  //Establish License
begin
  case fLicenseFile of
    saNone:
    begin
      szLicenseFile := '';
      fLicenseTypeString := '';
    end;
    saGPL:
    begin
      szLicenseFile := 'gpl.txt';
      fLicenseTypeString := 'GPL License';
    end;
    saLGPL:
    begin
      szLicenseFile := 'lgpl.txt';
      fLicenseTypeString := 'Library GPL License';
    end;
    saMIT:
    begin
      szLicenseFile := 'mit.txt';
      fLicenseTypeString := 'MIT License';
    end;
    saModifiedgpl:
    begin
      szLicenseFile := 'modifiedgpl.txt';
      fLicenseTypeString := 'Modified GPL License';
    end;
  end;
  case fMaskType of
    saNoMask: szMaskName := 'none';
    saUserImage: szMaskName := fMaskImageFilePath;
    saRoundedRect: szMaskName := 'roundedrect';
    saBigCog: szMaskName := 'bigcog';
    saBigFlower: szMaskName := 'bigflower';
    saBigSplash: szMaskName := 'bigsplash';
  end;

  // Temporarily create the form and controls
  fSplashForm := TForm.CreateNew(nil);
  if (fCreateType = saApplication) then
  begin
    fTitleString := Application.Title;
    fIcon := Application.Icon;
  end;
  if (fCreateType = saForm) then
  begin
    fTitleString := fFormTitleString;
    fIcon := fFormIcon;
  end;
  // The created form is parent to all the controls
  bevel := TBevel.Create(fSplashForm);
  okbutton := TBitBtn.Create(fSplashForm);
  LicenseButton := TBitBtn.Create(fSplashForm);
  Delaytimer := TTimer.Create(fSplashForm);
  Scrolltimer := TTimer.Create(fSplashForm);
  lbl_Title := TLabel.Create(fSplashForm);
  lbl_VersionInfo := TLabel.Create(fSplashForm);
  img_icon := TImage.Create(fSplashForm);
  img_background := TImage.Create(fSplashForm);
  MyBitmap := TBitMap.Create;
  iFormHeight := C_DEFAULTSPLASHHEIGHT;
  {$IFDEF LINUX}
  iFormHeight := C_DEFAULTSPLASHHEIGHT_LINUX;
{$ENDIF}
  iFormWidth := C_DEFAULTSPLASHWIDTH;

  // Now set positions and properties
  try  //.. finally FreeAndNil everything
    with fSplashForm do
    begin
      // Form
      position := poScreenCenter;
      if fFormType = fAbout then
        borderstyle := bsToolWindow
      else
        borderstyle := bsnone;
      Caption := 'About ' + fTitleString;
      formstyle := fsSystemStayOnTop;
      color := fBackGroundColor;
      Height := iFormHeight;
      Width := iFormWidth;



      // Shaped form?
      // Form is sized to mask image (MUST be BMP file)
      // Text is centred in a 320 x 240 invisible frame
      {$IFNDEF LINUX}// Problem with Canvas.Draw in 64-bit linux!

      // Skip this mask code block if MaskType is saNoMask
      if (fFormType = fSplash) and (fMaskType <> saNoMask) then
      begin
        // Deal with user-supplied image first
        if (fMaskType = saUserImage) then
          if (FileExists(fMaskImageFilePath)) then
          begin
            // Try to convert a jpg file if specified as such
            if ExtractFileExt(fMaskImageFilePath) = '.jpg' then
              if MakeBMPfromJPG(fMaskImageFilePath) = False then
                MessageDlg('SplashAbout Error',
                  'There is something wrong with the MaskImage File',
                  mtError, [mbOK], 0)
              else
                MessageDlg('SplashAbout',
                  'The MaskImage should be a .BMP file.  Your jpg has been converted and saved as a bmp.  Please amend the property.',
                  mtInformation, [mbOK], 0);
            // Load the user image into the BitMap
            MyBitMap.LoadFromFile(fMaskImageFilePath);
          end
          else
            // Looks like the specified user image isn't in the application folder
          begin
            MessageDlg('SplashAbout Error',
              'Cannot find MaskImage ' + fMaskImageFilePath,
              mtError, [mbOK], 0);
            exit;
          end;

        // Stock image specified
        if (fMaskType <> saUserImage) then
          MyBitmap.LoadFromLazarusResource(szMaskName);

        // Now to use the loaded BitMap
        try
          if MyBitMap.Height >= iFormHeight then
            iFormHeight := MyBitMap.Height;
          if MyBitMap.Width >= iFormWidth then
            iFormWidth := MyBitMap.Width;
          MyBitMap.Transparent := True;
          MyBitMap.TransparentColor := clBlack;
          Height := iFormHeight;
          Width := iFormWidth;
          Canvas.Draw(0, 0, MyBitMap);
          // raises Floating Point Error in 64-bit Nix (!??)
          SetShape(MyBitMap);
          bevel.Visible := False;
        except
          On e: Exception do
            MessageDlg('SplashAbout Error',
              'There is something wrong with the MaskImage File' +
              LineEnding + 'Error detail: ' + e.message,
              mtError, [mbOK], 0);
        end;
      end;
{$ENDIF}

      // bevel
      // Controls are placed relative to the bevel window
      bevel.Width := C_DEFAULTSPLASHWIDTH;
      bevel.Height := C_DEFAULTSPLASHHEIGHT;
      {$IFDEF LINUX}
      bevel.Height := C_DEFAULTSPLASHHEIGHT_LINUX;
{$ENDIF}

      if iFormHeight > bevel.Height then
        bevel.Top := (iFormHeight - bevel.Height) div 2
      else
        bevel.Top := 0;
      if iFormWidth > bevel.Width then
        bevel.Left := (iFormWidth - bevel.Width) div 2
      else
        bevel.Left := 0;
      bevel.BorderSpacing.Around := 4;
      bevel.BorderSpacing.InnerBorder := 4;
      bevel.Parent := fSplashForm;
      bevel.onClick := @CloseForm;


      // Close Button
      if fFormType = fAbout then
      begin
        okbutton.Kind := bkClose;
        okbutton.left := (Width div 2) - okbutton.Width div 2;
        okbutton.top := Height - okbutton.Height - 10;
        okbutton.parent := fSplashForm;
      end;

      // Delay Timer
      if fFormType = fSplash then
      begin
        if fDelaySeconds = 0 then
          fCloseOnIdle := True
        else
          fCloseOnIdle := False;
        if FCloseOnIdle then
          Application.AddOnIdleHandler(@ApplicationOnIdle)
        else
        begin
          // Fix negative values
          if fDelaySeconds < 1 then
            fDelaySeconds := 1;
          // Fix developer mistakenly specifying milliseconds
          if fDelaySeconds > 1000 then
            fDelaySeconds := fDelaySeconds div 1000;
          delaytimer.Interval := fDelaySeconds * 1000;
          delaytimer.OnTimer := @CloseForm;
          delaytimer.Enabled := True;
        end;
      end;


      // Icon
      img_icon.Width := 32;
      img_icon.Height := 32;
      img_icon.Top := bevel.Top + 20;
      img_icon.Left := bevel.left + (bevel.Width - 32 - 20);
      img_icon.Stretch := True;
      fIcon := Application.Icon; // Initialises the Icon object
      try
        if FileExists(fIconFilePath) then
        begin
          fIcon.LoadFromFile(fIconFilePath);
        end;
        if fIcon <> nil then
          img_icon.Picture.Icon := fIcon
        else
          img_icon.Picture.Icon := Application.Icon;
        img_icon.Parent := fSplashForm;
      except
        On e: Exception do
          MessageDlg('SplashAbout Error', 'There is something wrong with the Icon File',
            mtError, [mbOK], 0);
      end;
      // BackGround
      if FileExists(fBackGroundImageFilePath) then
      begin
        img_background.Align := alClient;
        img_background.Stretch := True;
        try
          img_background.Picture.LoadFromFile(fBackGroundImageFilePath);
          img_background.Parent := fSplashForm;
          img_background.SendToBack;
        except
          On e: Exception do
            MessageDlg('SplashAbout Error',
              'There is something wrong with the BackgroundImage', mtError, [mbOK], 0);
        end;
      end;

      // Title
      if fFormType = fSplash then
      begin
        lbl_Title.Top := bevel.Top + 64;
        lbl_Title.Left := bevel.Left;
        lbl_Title.AutoSize := False;
        lbl_Title.Width := bevel.Width;
        lbl_Title.Font.Size := 14;
        lbl_Title.Font.Style := [fsBold];
        lbl_Title.Height := 32;
        lbl_Title.Alignment := taCenter;
        if (fTitleString = '') then
          lbl_Title.Caption := Application.Title
        else
          lbl_Title.Caption := fTitleString;
        lbl_Title.Parent := fSplashForm;
      end;

      // License Button
      if (fFormType = fAbout) and (LazarusResources.Find(szLicenseFile) <> nil) then
      begin
        LicenseButton.Top := okButton.Top;
        LicenseButton.Caption := 'License...';
        LicenseButton.left := Width - LicenseButton.Width - 10;
        LicenseButton.OnClick := @ShowLicense;
        LicenseButton.Parent := fSplashForm;
      end;

      // Version Info
      lbl_VersionInfo.Autosize := False;
      lbl_VersionInfo.WordWrap := True;
      lbl_VersionInfo.Left := bevel.left + 20;
      lbl_VersionInfo.Width := bevel.Width - 50;
      lbl_VersionInfo.Height := bevel.Height - 50;
      lbl_VersionInfo.Alignment := taCenter;

      sVersionInfoString := '';
      if fFormType = fAbout then
        sVersionInfoString += fTitleString;
      if fCreditString <> '' then
        sVersionInfoString += LineEnding + fCreditString;
      // Show description or VersionInfo?
      if (fShowDescription = True) and (fDescription <> '') then
        sVersionInfoString += fAppVersionString + LineEnding + fDescription
      else
        sVersionInfoString += LineEnding + fVersionInfoString;

      if fLicenseTypeString <> '' then
        sVersionInfoString += LineEnding + LineEnding + 'Released under ' +
          fLicenseTypeString;
      lbl_VersionInfo.Caption := sVersionInfoString;
      if fFormType = fSplash then
        lbl_VersionInfo.Top :=
          Bevel.Top + (bevel.Height div 2) - 30
      else
        lbl_VersionInfo.Top := 40;
      lbl_VersionInfo.Parent := fSplashForm;
      lbl_VersionInfo.onClick := @CloseForm;


      // Now show the completed form
      Application.ProcessMessages;
      ShowModal;
      //      Application.ProcessMessages;
    end;
  finally
    // Controls normally destroyed with parent
    // but if Try block fails, ensure no memory leaks
    FreeAndNil(bevel);
    FreeAndNil(img_icon);
    FreeAndNil(img_background);
    FreeAndNil(lbl_Title);
    FreeAndNil(lbl_VersionInfo);
    FreeAndNil(okbutton);
    FreeAndNil(LicenseButton);
    FreeAndNil(delaytimer);
    FreeAndNil(Scrolltimer);
    FreeAndNil(MyBitMap);
    FreeAndNil(fSplashForm);
  end;
end;

initialization
{$I license.lrs}
{$I masks.lrs}

end.
