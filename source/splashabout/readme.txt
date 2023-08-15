SplashAbout component for Lazarus
minesadorada@charcodelvalle.com
============================

Installation
========
Make a new folder 'splashabout' in your lazarus/components folder
Copy the contents of the zip file into it
In Lazarus, choose the menu 'Package/Install/Uninstall Packages'
From the right-hand column, select 'splashabout 1.0' and click 'Install selection'

Lazarus will ask you whether to 'recompile the IDE'  - answer 'yes'

When all is done, click the 'Additional' component palette to see the SplashAbout component.

Use
===
Add the component to your form (SplashAbout1)

To show a splash screen:
In the FormCreate event, use this code:
procedure TForm1.FormCreate(Sender: TObject);
begin
  SplashAbout1.ShowSplash;
end; 

To show the 'About' dialog, use this code:
procedure TForm1.Button1Click(Sender: TObject);
begin
  SplashAbout1.ShowAbout;
end;    

Tweaking
=======
See the 'testapp' example project to experiment