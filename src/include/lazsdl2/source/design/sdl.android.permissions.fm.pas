unit SDL.Android.Permissions.Fm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, IDEOptionsIntf, ExtCtrls;

type

  { TfrmAndroidPermissions }

  TfrmAndroidPermissions = class(TAbstractIDEOptionsEditor)
    chbPermissions: TCheckGroup;
  private
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

uses
  SDL.Android.Options.Project;

type

  { TString }

  TString = class
  public
    Value: String;
    constructor Create(const AValue: String); reintroduce;
  end;

{ TString }

constructor TString.Create(const AValue: String);
begin
  Value := AValue
end;

{ TfrmAndroidPermissions }

function TfrmAndroidPermissions.GetTitle: String;
begin
  Result := 'Permissions'
end;

procedure TfrmAndroidPermissions.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
end;

procedure TfrmAndroidPermissions.ReadSettings(AOptions: TAbstractIDEOptions);
var
  PermDesc, PermName: String;
  i, Index: Integer;
begin
  if AOptions is TAndroidProjectOptions then with AOptions as TAndroidProjectOptions do
  begin
    chbPermissions.Items.Clear;
    (chbPermissions.Items as TStringList).OwnsObjects := True;
    for i := 1 to AllPermissions.Count - 1 do
    begin
      AllPermissions.GetNameValue(i - 1, PermName, PermDesc);
      Index := chbPermissions.Items.AddObject(PermDesc, TString.Create(PermName));
      chbPermissions.Checked[Index] := Permissions.IndexOf(PermName) > -1
    end
  end
end;

procedure TfrmAndroidPermissions.WriteSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
begin
  if AOptions is TAndroidProjectOptions then with AOptions as TAndroidProjectOptions do
  begin
    Permissions.Clear;
    for i := 1 to chbPermissions.Items.Count - 1 do
    begin
      if chbPermissions.Checked[i] then
        Permissions.Add(TString(chbPermissions.Items.Objects[i]).Value)
    end;

    SaveOptions;
  end;
end;

class function TfrmAndroidPermissions.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TAndroidProjectOptions
end;

end.

