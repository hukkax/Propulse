unit SDL2.PropEdits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits;

type

  TSDLClassPropEdit = class(TClassPropertyEditor)
  public
  end;

  { TSDLLibraryProviderEditor }

  TSDLLibraryProviderEditor = class(TComponentOneFormPropertyEditor)
  public
    procedure AfterConstruction; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
    function GetValue: AnsiString; override;
    function GetVisualValue: ansistring; override;
  end;

implementation

uses
  typinfo, SDL.Extended.Interfaces, SDL.Extended.LibraryProvider, ObjInspStrConsts;

{ TSDLLibraryProviderEditor }

procedure TSDLLibraryProviderEditor.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TSDLLibraryProviderEditor.GetValues(Proc: TGetStrProc);
var
  Cmp: TComponent;
  i: Integer;
begin
  Proc(oisNone);

  if GetInstProp^.Instance is TComponent then
  begin
    Cmp := GetInstProp^.Instance as TComponent;

    if Supports(Cmp.Owner, IInternalProviderContainer) then
      Proc('Internal');

    if Assigned(Cmp.Owner) then
    begin
      for i := 0 to Cmp.Owner.ComponentCount - 1 do
      begin
        if Cmp.Owner.Components[i] is TSDLCustomLibraryProvider then
          Proc(Cmp.Owner.Components[i].Name);
      end
    end;
  end;
end;

procedure TSDLLibraryProviderEditor.SetValue(const NewValue: ansistring);
var
  Cmp: TComponent;
  DotPos: Int32;
  OwnerName, ObjName: String;
  Provider: IInternalProviderContainer;
begin
  if GetInstProp^.Instance is TComponent then
  begin
    Cmp := GetInstProp^.Instance as TComponent;

    DotPos := Pos('.', NewValue);
    if DotPos > 0 then
    begin
      OwnerName := Copy(NewValue, 1, DotPos - 1);
      ObjName := Copy(NewValue, DotPos + 1, Length(NewValue));
    end
    else
    begin
      OwnerName := '';
      ObjName := NewValue;
    end;

    if (ObjName = 'Internal') and (Cmp.Owner.GetInterface(IInternalProviderContainer, Provider)) then
      SetObjectProp(Cmp, GetName, Provider.GetInternalProvider)
    else
    if (OwnerName = Cmp.Name) and (Cmp.GetInterface(IInternalProviderContainer, Provider)) then
      SetObjectProp(Cmp, GetName, Provider.GetInternalProvider)
    else
    if Assigned(Cmp.Owner) then
      SetObjectProp(Cmp, GetName, Cmp.Owner.FindComponent(ObjName))
    else
      SetObjectProp(Cmp, GetName, nil)
  end;
  Modified;
end;

function TSDLLibraryProviderEditor.GetValue: AnsiString;
var
  Provider: TComponent;
begin
  if GetInstProp^.Instance is TComponent then
  begin
    Provider := GetObjectValue as TComponent;

    if Assigned(Provider) then
      Result := Provider.Name
    else
      Result := ''
  end
  else
    Result := ''
end;

function TSDLLibraryProviderEditor.GetVisualValue: ansistring;
begin
  Result := GetValue
end;

end.

