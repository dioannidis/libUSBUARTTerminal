{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit splashabout;

{$warn 5023 off : no warning about unused units}
interface

uses
  usplashabout, uversion, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('usplashabout', @usplashabout.Register);
end;

initialization
  RegisterPackage('splashabout', @Register);
end.
