unit CWE.MainMenu;

{$mode delphi}

interface

uses
	Classes, SysUtils, Types,
	ShortcutManager,
	CWE.Core, CWE.Widgets.Text;

type
	TCWEMainMenuList = class(TCWETwoColumnList)
	public
		function	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
	end;

	TCWEMainMenu = class
	private
		Section: 	TKeyBindings;
		List:		TCWEMainMenuList;
	public
		procedure	Show;
		procedure	MainMenuCommand(Sender: TCWEControl);

		procedure 	SetSection(var ASection: TKeyBindings);
		procedure 	AddSection(const Caption: AnsiString);
		procedure 	AddCmd(Key: Cardinal; const Caption: AnsiString);
		procedure 	AddCmdEx(Key: Cardinal; const Caption: AnsiString);
	end;

var
	ContextMenu: TCWEMainMenu;

implementation

uses
	MainWindow,
	TextMode, SDL2,
	ProTracker.Util, CWE.Dialogs;

{ TCWEMainMenu }

procedure TCWEMainMenu.SetSection(var ASection: TKeyBindings);
begin
	Section := ASection;
end;

procedure TCWEMainMenu.AddSection(const Caption: AnsiString);
begin
	List.Items.Add(TCWEListItem.Create(Caption, LISTITEM_HEADER, nil, 3, 2));
end;

procedure TCWEMainMenu.AddCmd(Key: Cardinal; const Caption: AnsiString);
begin
	List.Items.Add(TCWEListItem.Create(
		Caption + COLUMNSEPARATOR + ShortCuts.GetShortcut(Section, Key),
		Ord(Key), Pointer(Section)));
end;

procedure TCWEMainMenu.AddCmdEx(Key: Cardinal; const Caption: AnsiString);
begin
	List.Items.Add(TCWEListItem.Create(Caption, $80000000 + Key, Pointer(Section)));
end;

procedure TCWEMainMenu.Show;
var
	Dlg: TCWEScreen;
	Section: TKeyBindings;
var
	i, W, H: Integer;
begin
	W := 34+6;
	H := 32;

	if (ModalDialog.Dialog <> nil) or (CurrentScreen = nil) then
		Exit;

	Dlg := ModalDialog.CreateDialog(DIALOG_CONTEXTMENU, Bounds(
		(Console.Width  div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H),
		 'Menu');

	List := TCWEMainMenuList.Create(Dlg, '', 'Menu',
		Types.Rect(1, 2, W-1, H-1), True);

	with List do
	begin
		ColorBack := TConsole.COLOR_PANEL;
		ColorFore := TConsole.COLOR_TEXT;
		ColumnColor[0] := ColorFore;
		ColumnColor[1] := TConsole.COLOR_3DDARK;
		ColumnWidth[1] := 13;
		ColumnWidth[0] := Width - ColumnWidth[1];
		OnActivate  := MainMenuCommand;
		Selection3D := True;
		Data[0].Value := TConsole.COLOR_LIGHT; // bright white
		for i := 1 to 3 do
			Data[i].Value := 8; // hover bg + border
	end;

	if CurrentScreen.OnContextMenu then
		Window.OnContextMenu;

	H := List.Items.Count + 3;
	ModalDialog.SetBounds(Bounds(
		(Console.Width  div 2) - (W div 2),
		(Console.Height div 2) - (H div 2), W, H));
	List.SetBounds(Types.Rect(1, 2, W-1, H-1));
	List.ItemIndex := 1;
	List.Scrollbar.Visible := False;

	ModalDialog.Show;
end;

procedure TCWEMainMenu.MainMenuCommand(Sender: TCWEControl);
var
	Item: TCWEListItem;
	Sect: TKeyBindings;
	Binding: TKeyBinding;
	i: Integer;
	Cmd: Cardinal;
	Key: Integer;
	Shift: TShiftState;
begin
	// get the Key and Shift codes for a keybinding const (e.g. keyScreenHelp -> F1, [])

	Item := List.Items[List.ItemIndex];
	if Item.ObjData = nil then Exit;
	Cmd := Item.Data;

	if (Cmd and $80000000) = 0 then
	begin
		i := Shortcuts.Sections.IndexOf(TKeyBindings(Item.ObjData));
		if i < 0 then
		begin
			Log(TEXT_WARNING+'Section not found!');
			Exit;
		end;

		Sect := Shortcuts.Sections[i];
		Binding := Sect.FindKey(Cmd);

		if Binding = nil then
		begin
			Log(TEXT_WARNING+'Binding not found!');
			Exit;
		end
		else
		begin
			Key := Binding.Shortcut.Key;
			Shift := Binding.Shortcut.Shift;
			if Key <> 0 then
			begin
				ModalDialog.Close;
				Window.OnKeyDown(Key, Shift);
			end
			else
				Log(TEXT_WARNING+'Unhandled command!');
		end;
	end
	else
	begin
		ModalDialog.Close;
		CurrentScreen.HandleCommand(Cmd and $7FFFFFFF);
	end;
end;

function TCWEMainMenuList.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
begin
	if Key = SDLK_ESCAPE then
	begin
		Result := True;
		ModalDialog.Close;
	end
	else
		Result := inherited;
end;


end.

