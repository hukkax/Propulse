unit SDL.Android.JavaSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, Forms;

resourcestring
  FileDescNameJava = 'Java source';
  FileDescJava = 'Java source file for use in android project';

type

  { TJavaSourceFile }

  TJavaSourceFile = class(TProjectFileDescriptor)
  public
    constructor Create; override;
    function CreateSource(const aFilename, aSourceName, aResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

implementation

uses
  Dialogs, LCLProc;

{ TJavaSourceFile }

constructor TJavaSourceFile.Create;
begin
  inherited Create;
  Name := 'JavaSource';
  DefaultFilename := 'JavaClass.java';
  AddToProject := True;
end;

function TJavaSourceFile.CreateSource(const aFilename, aSourceName,
  aResourceName: string): string;
var
  JavaClassName: String;
  Source: TStringList;
begin
  JavaClassName := '';

  while Trim(JavaClassName) = '' do
    JavaClassName := Trim(InputBox('New Java class', 'Java class name', 'NewClass'));

  DefaultFilename := JavaClassName + '.java';

  Source := TStringList.Create;
  try
    Source.Add('');
    Source.Add('public class ' + JavaClassName + ' {');
    Source.Add('');
    Source.Add('}');
    Source.Add('');

    Result := Source.Text
  finally
    Source.Free
  end
end;

function TJavaSourceFile.GetLocalizedName: string;
begin
  Result := FileDescNameJava
end;

function TJavaSourceFile.GetLocalizedDescription: string;
begin
  Result := FileDescJava
end;

end.

