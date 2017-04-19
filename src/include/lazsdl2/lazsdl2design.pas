{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazSDL2Design;

interface

uses
  SDL.Android.JavaSource, SDL.Android.Options.Enviorment, 
  SDL.Android.Options.Enviorment.Fm, SDL.Android.Options.Libraries.Fm, 
  SDL.Android.Options.Project, SDL.Android.Options.Project.Fm, 
  SDL.Android.Permissions.Fm, SDL.Android.ProjectBuilder, 
  SDL.Android.SourceTemplates.Fm, SDL.ProjectBuilder, SDL2.DesignStrings, 
  SDL2.NewProject, SDL2.Project, SDL2.PropEdits, SDL2.Register, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SDL2.Register', @SDL2.Register.Register);
end;

initialization
  RegisterPackage('LazSDL2Design', @Register);
end.
