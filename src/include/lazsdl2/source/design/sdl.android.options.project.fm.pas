unit SDL.Android.Options.Project.Fm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ActnList,
  Dialogs, IDEOptionsIntf;

type

  { TfrmAndroidProjectOptions }

  TfrmAndroidProjectOptions = class(TAbstractIDEOptionsEditor)
    cmbTargetMinSDK: TComboBox;
    cmbTargetSDK: TComboBox;
    edtProjectName: TEdit;
    lblAndroidVersion: TLabel;
    lblMinAndroidVersion: TLabel;
    lblProjectName: TLabel;
    lblTarget: TLabel;
    lblTargetMinSDK: TLabel;
    lsScreens: TCheckGroup;
    cmbOpenGLEs: TComboBox;
    edtMain: TEdit;
    edPackageName: TEdit;
    lblMain: TLabel;
    lblOpenGLEs: TLabel;
    lblPackageName: TLabel;
    procedure cmbTargetMinSDKSelect(Sender: TObject);
    procedure cmbTargetSDKSelect(Sender: TObject);
  private
    procedure InternalFillTargets;
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
  SDL.Android.Options.Project, SDL.Android.Options.Enviorment;

{ TfrmAndroidProjectOptions }

procedure TfrmAndroidProjectOptions.cmbTargetMinSDKSelect(Sender: TObject);
var
  Options: TAndroidEnviormentOptions;
begin
  Options := TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance);
  lblMinAndroidVersion.Caption := '-' + Options.TargetVersion(cmbTargetMinSDK.Text)
end;

procedure TfrmAndroidProjectOptions.cmbTargetSDKSelect(Sender: TObject);
var
  Options: TAndroidEnviormentOptions;
begin
  Options := TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance);
  lblAndroidVersion.Caption := '-' + Options.TargetVersion(cmbTargetSDK.Text)
end;

procedure TfrmAndroidProjectOptions.InternalFillTargets;
var
  EnvOptions: TAndroidEnviormentOptions;
  ProjOptions: TAndroidProjectOptions;
begin
  EnvOptions := TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance);
  ProjOptions := TAndroidProjectOptions(TAndroidProjectOptions.GetInstance);

  EnvOptions.FillTargets(cmbTargetSDK.Items);
  EnvOptions.FillTargets(cmbTargetMinSDK.Items);
  cmbTargetSDK.ItemIndex := cmbTargetSDK.Items.IndexOf(ProjOptions.TargetSDK);
  cmbTargetMinSDK.ItemIndex := cmbTargetMinSDK.Items.IndexOf(ProjOptions.TargetMinSDK);
  cmbTargetSDK.OnSelect(nil);
  cmbTargetMinSDK.OnSelect(nil);
end;

function TfrmAndroidProjectOptions.GetTitle: String;
begin
  Result := 'General'
end;

procedure TfrmAndroidProjectOptions.Setup(ADialog: TAbstractOptionsEditorDialog);
begin

end;

procedure TfrmAndroidProjectOptions.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions is TAndroidProjectOptions then with AOptions as TAndroidProjectOptions do
  begin
    edPackageName.Text := PackageName;
    cmbOpenGLEs.ItemIndex := cmbOpenGLEs.Items.IndexOf(OpenGLES);
    edtMain.Text := MainActivity;
    lsScreens.Checked[0] := ScreensSmall;
    lsScreens.Checked[1] := ScreensNormal;
    lsScreens.Checked[2] := ScreensLarge;
    lsScreens.Checked[3] := ScreensxLarge;
    lsScreens.Checked[4] := ScreensAnyDensity;
    edtProjectName.Text := ProjectName;
    InternalFillTargets;
  end;
end;

procedure TfrmAndroidProjectOptions.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions is TAndroidProjectOptions then with AOptions as TAndroidProjectOptions do
  begin
    PackageName := edPackageName.Text;
    OpenGLES := cmbOpenGLEs.Text;
    MainActivity := edtMain.Text;
    ScreensSmall := lsScreens.Checked[0];
    ScreensNormal := lsScreens.Checked[1];
    ScreensLarge := lsScreens.Checked[2];
    ScreensxLarge := lsScreens.Checked[3];
    ScreensAnyDensity := lsScreens.Checked[4];
    TargetMinSDK := cmbTargetMinSDK.Text;
    TargetSDK := cmbTargetSDK.Text;
    ProjectName := edtProjectName.Text;
    SaveOptions;
  end;
end;

class function TfrmAndroidProjectOptions.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TAndroidProjectOptions
end;

end.

