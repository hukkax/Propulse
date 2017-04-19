unit SDL2.NewProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type

  { TfrmNewProject }

  TfrmNewProject = class(TForm)
    chbIncludeLibs: TCheckBox;
    pnlControl: TButtonPanel;
    edtClassName: TEdit;
    edtTitle: TEdit;
    lblClassName: TLabel;
    lblTitle: TLabel;
    procedure edtClassNameKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TfrmNewProject }

procedure TfrmNewProject.edtClassNameKeyPress(Sender: TObject; var Key: char);
const
  Characters = ['a'..'z', 'A'..'Z'];
  Numbers = ['0'..'9'];
  Misc = ['_', #8, #9, #27];
  AllowedKeys = Characters + Numbers + Misc;
begin
  if not (Key in AllowedKeys) then
    Key := #0;
end;

end.

