//
// ConsoleWidgetEngine: textual widgets
// by hukka 2016
//
unit CWE.Widgets.Text;

interface

uses
	Classes, Types, SysUtils, SDL2, ShortcutManager,
	Generics.Collections,
	CWE.Core, CWE.Widgets.Scrollers;

const
	COLUMNSEPARATOR = #255;

	LISTITEM_HEADER = MAXINT;

	ALIGN_LEFT   = 0;
	ALIGN_CENTER = 1;
	ALIGN_RIGHT  = 2;
	ALIGN_HEADER = 3;

	MAXCOLUMNS   = 3;		// max columns for list widget, 0-based

type
	TCWEListItem = class; 	// forward

	TCWEListItemPaintEvent	= 	procedure (Sender: TCWEControl; var Item: TCWEListItem;
								Index: Word; P: TPoint) of Object;

	TCWELabel = class(TCWEControl)
	public
		Alignment: 	Byte;
		Sunken:		Boolean;

		procedure 	SetCaption(const NewCaption: AnsiString);

		procedure 	Paint; override;
		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
	end;

	TCWESunkenLabel = class(TCWELabel)
	public
		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
	end;

	TCWEURLLabel = class(TCWELabel)
	public
		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
		//procedure 	Paint; override;
		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
	end;

	TCWEScreenHelper = class helper for TCWEScreen
	private
		function 	GenerateHeaderText(const KeyName: AnsiString = ''): AnsiString;
	public
		function	AddHeader(const KeyName: AnsiString = ''): TCWELabel;
		function	UpdateHeader(const KeyName: AnsiString = ''): TCWELabel;
	end;

	TCWEEdit = class(TCWEControl)
	public
		Cursor: 	TPoint;
		Offset,
		MaxLength: 	Word;
		ReportAnyChange: Boolean; // if False, only triggers change callback on Return key
		AllowedChars: AnsiString;

		procedure 	SetCaption(const NewCaption: AnsiString; CursorAtStart: Boolean = False);

		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	TextInput(var Key: Char): Boolean; override;

		procedure 	Paint; override;
		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
	end;

	TCWEButton = class(TCWEControl)
	public
		Tag:			Integer;
		ModalResult:	SmallInt;
		Shortcut:		ShortcutManager.TShortCut;

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
		procedure 	Paint; override;

		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
	end;

	TCWEListItem = class
	public
		Captions:	array[0..MAXCOLUMNS] of String;
		Data:		Cardinal;
		ObjData: 	Pointer;
		ColorFore,
		ColorBack:  ShortInt;

		constructor Create(const Text: String;
					DataVal: Cardinal = 0; DataPtr: Pointer = nil;
					colFg: ShortInt = -1; colBg: ShortInt = -1);
	end;

	TCWEList = class(TCWEScrollableControl)
	private
		FOnPaintItem:	TCWEListItemPaintEvent;
		FOnActivate:	TCWENotifyEvent;

		PrevClickTime:	TDateTime;

		procedure 		FixOffset(Center: Boolean = False);
	public
		Columns:		Byte;
		ColumnWidth,
		ColumnX,
		ColumnColor:	array [0..MAXCOLUMNS] of ShortInt;
		ItemIndex:		Integer;
		Items:			TObjectList<TCWEListItem>;
		Selection3D:	Boolean;

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
		destructor  Destroy; override;

		procedure 	Clear;
		function 	GetCaption(index: Byte = 0): String; virtual;
		function 	Select(Index: Integer;
					Center: Boolean = True): Integer; overload;
		function 	Select(const ItemCaption: String;
					ItemColumn: Byte = 0; Center: Boolean = True): Integer; overload;
		function	AddItem(const Text: String): TCWEListItem;
		procedure 	AdjustScrollbar;

		function 	ProcessShortcut(Sc: ControlKeyNames): Boolean; override;

		function	MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean; override;
		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		procedure 	MouseMove(X, Y: Integer; P: TPoint); override;
		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;

		property	OnPaintItem: TCWEListItemPaintEvent read FOnPaintItem write FOnPaintItem;
		property	OnActivate:  TCWENotifyEvent read FOnActivate write FOnActivate;

		procedure 	Paint; override;
	end;

	TCWETwoColumnList = class(TCWEList)
	public
		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
	end;

	TCWEMemo = class(TCWEScrollableControl)
	private
		procedure 	AdjustScrollbar;
	public
		Lines:		TStringList;

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
		destructor  Destroy; override;

		procedure 	Add(const S: AnsiString; Color: ShortInt = -1; Center: Boolean = False);

		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean; override;
		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;

		procedure 	Paint; override;
	end;

implementation

uses
	MainWindow, DateUtils,
	{$IFDEF WINDOWS}Windows, ShellAPI,{$ENDIF}
	TextMode, Math,
	ProTracker.Util;

// ==========================================================================
{ TCWELabel }
// ==========================================================================

constructor TCWELabel.Create;
begin
	inherited;

	WantMouse := False;
	WantKeyboard := False;
	WantHover := False;

	//ColorBack := -1;//TConsole.COLOR_PANEL;
	ColorFore := TConsole.COLOR_TEXT;

	SetBorder(False, False, True, True);
end;

procedure TCWELabel.Paint;
var
	S: String;
	C: Byte;
begin
	inherited; // paint borders

	if (WantHover) and (Hovered) then
		C := TConsole.COLOR_LIGHT
	else
		C := ColorFore;

	Console.FillRect(Rect, ' ', C, ColorBack);
	S := Copy(Caption, 1, Width+1);

	case Alignment of

		ALIGN_LEFT:
			Console.Write(S, Rect.Left, Rect.Top);

		ALIGN_CENTER:
			Console.Write(S, ((Rect.Right-Rect.Left) div 2) - (Length(S) div 2) + Rect.Left, Rect.Top);

		ALIGN_RIGHT:
			Console.Write(S, Rect.Right - Length(S), Rect.Top);

		ALIGN_HEADER:
			Console.WriteHeader(S, Rect.Top, C, ColorBack, Rect.Left, Rect.Right);

	end;
end;

procedure TCWELabel.SetCaption(const NewCaption: AnsiString);
begin
	Caption := NewCaption;
	if Screen.Active then Paint;
end;

// ==========================================================================
{ TCWESunkenLabel }
// ==========================================================================

constructor TCWESunkenLabel.Create;
begin
	inherited;

	SetBorder(True, False, True, True);
	ColorBack := TConsole.COLOR_BLANK;
	ColorFore := 5;
end;

// ==========================================================================
{ TCWEURLLabel }
// ==========================================================================

constructor TCWEURLLabel.Create(Owner: TCWEControl; const sCaption,
  sID: AnsiString; const Bounds: TRect; IsProtected: Boolean);
begin
	inherited;

	WantMouse := True;
	WantKeyboard := False;
	WantHover := True;

	ColorFore := TConsole.COLOR_LINK;
end;

function TCWEURLLabel.MouseDown(Button: TMouseButton; X, Y: Integer;
	P: TPoint): Boolean;
begin
	{$IFDEF WINDOWS}
	if Button = mbLeft then
		ShellExecute(0, 'open', PChar(String(Caption)), nil, nil, SW_SHOW);
	{$ENDIF}
	Result := inherited;
end;

// ==========================================================================
{ TCWEButton }
// ==========================================================================

constructor TCWEButton.Create;
begin
	inherited;

	ColorFore := TConsole.COLOR_TEXT;

	Shortcut.Key := 0;
	Shortcut.Shift := [];

	WantMouse := True;
	WantKeyboard := True;
	WantHover := True;
end;

function TCWEButton.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
var
	Sc: ControlKeyNames;
begin
	if not WantKeyboard then Exit(False);

	Result := True;
	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, Shift));
	case Sc of
		ctrlkeyRETURN:
			if Assigned(FOnChange) then
				FOnChange(Self);
		ctrlkeyLEFT, ctrlkeyUP:
			Screen.BrowseControls(True);
		ctrlkeyRIGHT, ctrlkeyDOWN:
			Screen.BrowseControls(False);
	else
		Result := inherited KeyDown(Key, Shift);
	end;
end;

function TCWEButton.MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
var
	Key: Integer;
	Shift: TShiftState;
begin
	Result := False;
	if (Focused) and (Hovered) then
	begin
		if Shortcut.Key <> 0 then
		begin
			Key := Shortcut.Key;
			Shift := Shortcut.Shift;
			Window.OnKeyDown(Key, Shift);
			Result := True;
		end;
		if Assigned(FOnChange) then
		begin
			Result := True;
			FOnChange(Self);
		end;
	end;
end;

procedure TCWEButton.Paint;
var
	colF: Byte;
begin
	if not Screen.Active then Exit;

	inherited;

	colF := ColorFore;

	if (Focused) and (Hovered) and (Screen.MouseInfo.Buttons[mbLeft]) then
	begin
		colF := TConsole.COLOR_3DLIGHT;
		Console.FrameRectPx(Rect, True);
	end
	else
	begin
		if (Hovered) {or (Focused)} then colF := TConsole.COLOR_3DLIGHT;
		if (Focused and WantKeyboard) then
		begin
			Console.FrameRectPx(Rect, False, False{True});
		end
		else
		begin
			// hack to remove the fat border from a previous draw op
			Console.FrameRectPx(Rect, False, True, -1, TConsole.COLOR_PANEL, TConsole.COLOR_PANEL);
			Console.FrameRectPx(Rect, False, False);
		end;
	end;

	Console.FillRect(Rect, ' ', colF, ColorBack);
	Console.Write(Copy(Caption, 1, Rect.Right - Rect.Left),
		Rect.Left + ((Rect.Right - Rect.Left) div 2) - (Length(Caption) div 2),
		Rect.Top + ((Rect.Bottom - Rect.Top)  div 2), colF, ColorBack);

	if (Focused and WantKeyboard) then
		Console.Bitmap.FrameRect(
			Console.GetPixelRect(Rect, 2),
			Console.Palette[TConsole.COLOR_TEXT]);
end;

(*
procedure TCWEWidgetEngine.SetControlInfo(Kind: TCWEControlClass;
	Want_Mouse, Want_Keyboard, Want_Hover: Boolean);
begin
{	with ControlInfo[Kind] do
	begin
		WantMouse    := Want_Mouse;
		WantKeyboard := Want_Keyboard;
		WantHover    := Want_Hover;
	end;}
end;
*)

// ==========================================================================
{ TCWEList }
// ==========================================================================

constructor TCWEList.Create;
var
	i: Integer;
begin
	inherited;

	FOnPaintItem := nil;
	PrevClickTime := 0;
	Columns := 1;

	Items := TObjectList<TCWEListItem>.Create(True);
	Offset := 0;
	SetBorder(True, True, True, False);

	ColorFore := TConsole.COLOR_3DLIGHT; //COL_LIST_FORE;
	for i := 0 to MAXCOLUMNS do
		ColumnColor[i] := -1;

	ColorBack := TConsole.COLOR_BLANK; //COL_LIST_BACK;
	Selection3D := False;

	SetData(0, TConsole.COLOR_TEXT,  'Selection background');
	SetData(1, TConsole.COLOR_LIGHT, 'Selection foreground');
	SetData(2, TConsole.COLOR_3DLIGHT, '3D Hovered Light Border');
	SetData(3, TConsole.COLOR_3DDARK,  '3D Hovered Dark Border');
	SetData(4, 255,  'Inactive selection foreground');
	SetData(5, 15,   'Inactive selection background');

	WantMouse := True;
	WantKeyboard := True;
	WantHover := False;

	CreateScrollbar;
end;

destructor TCWEList.Destroy;
begin
	if Assigned(Items) then
		Items.Free;
	inherited;
end;

function TCWEList.AddItem(const Text: String): TCWEListItem;
begin
	Result := TCWEListItem.Create(Text);
	Items.Add(Result);
	AdjustScrollbar;
end;

procedure TCWEList.AdjustScrollbar;
begin
	MaxScroll := Max(Items.Count - Height - 1, 0);
	if Assigned(Scrollbar) then
		Scrollbar.Adjust(Items.Count-1, Height);
end;

procedure TCWEList.Clear;
begin
	if Assigned(Items) then
		Items.Clear;
	Offset := 0;
	ItemIndex := 0;
	AdjustScrollbar;
end;

function TCWEList.GetCaption(index: Byte = 0): String;
begin
	if (Assigned(Items)) and (ItemIndex >= 0) and (ItemIndex < Items.Count) then
		Result := Items[ItemIndex].Captions[index]
	else
		Result := '';
end;

procedure TCWEList.FixOffset(Center: Boolean = False);
var
	os: Integer;

	function ItemInView: Boolean;
	begin
		Result := (ItemIndex >= os) and (ItemIndex < (os + Height));
	end;

begin
	if (Center) and (not ItemInView) then
		os := ItemIndex - (Height div 2)
	else
		os := Offset;

	if not ItemInView then
	begin
		if ItemIndex < os then
			os := ItemIndex
		else
		while (os + Height) < ItemIndex do
			Inc(os);
	end;

	MaxScroll := Max(Items.Count - Height - 1, 0);
	os := Min(os, MaxScroll);
	Offset := Max(os, 0);

	AdjustScrollbar;
end;

function TCWEList.ProcessShortcut(Sc: ControlKeyNames): Boolean;
var
	i: Integer;
	B: Boolean;
begin
	i := ItemIndex;
	B := False;

	case Sc of

		ctrlkeyUP:
			if ItemIndex > 0 then
				Dec(ItemIndex);

		ctrlkeyDOWN:
			if ItemIndex < Items.Count-1 then
				Inc(ItemIndex);

		ctrlkeyHOME:
			ItemIndex := 0;

		ctrlkeyEND:
			ItemIndex := Items.Count-1;

		ctrlkeyPGUP:
			ItemIndex := Max(0, ItemIndex - Height);

		ctrlkeyPGDN:
			ItemIndex := Min(ItemIndex + Height, Items.Count-1);

		ctrlkeyRETURN:
			B := True;

	else
		Exit(False);
	end;

	Result := True;
	FixOffset;
	Paint;

	if i <> ItemIndex then Change;

	// Call activation handler last because it may cause
	// this object and its parent screen to be freed
	if (B) and (Assigned(FOnActivate)) then
		FOnActivate(Self);
end;

function TCWEList.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
begin
	Result := inherited KeyDown(Key, Shift);
	if not Result then
		Result := ProcessShortcut(ControlKeyNames(
			Shortcuts.Find(ControlKeys, Key, Shift)));
end;

function TCWEList.MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean;
const
	ScrollAmount = 2;
begin
	Result := True;

	if Items.Count <= Height then
	begin
		Offset := 0;
		Exit;
	end;

	if WheelDelta < 0 then
		ScrollBy(ScrollAmount)
	else
	if WheelDelta > 0 then
		ScrollBy(-ScrollAmount);
end;

procedure TCWEList.MouseMove(X, Y: Integer; P: TPoint);
var
	i: Integer;
begin
	if (not Assigned(Screen)) or (not Selection3D) then Exit;

	i := P.Y + Offset;

	if (Selection3D) and (i <> ItemIndex) and (i < Items.Count) then
	begin
		ItemIndex := i;
		Paint;
	end;
end;

function TCWEList.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
var
	i: Integer;
	Key: Integer;
begin
	if (not Assigned(Screen)) or (Button = mbMiddle) then Exit(False);

	i := P.Y + Offset;

	if (not Selection3D) and (i <> ItemIndex) and (i < Items.Count) then
	begin
		ItemIndex := i;
		Change;
		PrevClickTime := Now;
	end
	else
	if Button = mbLeft then
	begin
		// detect double click (or activate item by single click if Selection3D)
		//
		if (Selection3D) or (MilliSecondsBetween(Now, PrevClickTime) < {GetDoubleClickTime}500) then
		begin
			PrevClickTime := Now;
			if Selection3D then
			begin
				ItemIndex := i;
				ProcessShortcut(ctrlkeyRETURN);
			end
			else
			begin
				Key := SDLK_RETURN;
				KeyDown(Key, []);
			end;
			Exit(True);
		end
		else
			PrevClickTime := Now;
	end;

	Result := True;
	Paint;
	if Button = mbRight then Result := False;
end;

function TCWEList.Select(Index: Integer; Center: Boolean = True): Integer;
begin
	if Index = ItemIndex then
		Exit(Index);

	if (Index >= 0) and (Index < Items.Count) then
	begin
		ItemIndex := Index;
		FixOffset(Center);
		Paint;
		Result := Index;
	end
	else
		Result := -1;
end;

function TCWEList.Select(const ItemCaption: String;
	ItemColumn: Byte = 0; Center: Boolean = True): Integer;
var
	i: Integer;
begin
	for i := 0 to Items.Count-1 do
		if Items[i].Captions[ItemColumn] = ItemCaption then
			Exit(Select(i, Center));
	Result := -1;
end;

procedure TCWEList.Paint;
var
	i, x, y, xx: Integer;
	cf, cb: SmallInt;
	SR: TRect;
	Item: TCWEListItem;
begin
	if not Screen.Active then Exit;

	Console.FillRect(Rect, ' ', ColorFore, ColorBack);

	inherited;	// paint border & scrollbar

	if (not Assigned(Items)) or (Items.Count < 1) then Exit;

	Columns := Min(Columns, MAXCOLUMNS+1);

	ColumnWidth[0] := Width + Columns;
	for x := Columns downto 1 do
		Dec(ColumnWidth[0], ColumnWidth[x] + 1);

	SR := Types.Rect(-1, -1, -1, -1);

	if not Selection3D then
	begin
		xx := Rect.Left;
		cf := Console.Font.Width;
		cb := Console.Font.Height;

		for x := 0 to Columns-1 do
		begin
			if x > 0 then // column separator
				Console.Bitmap.VertLine(xx * cf - (cf div 2) - 1, Rect.Top * cb,
					(Rect.Top + Height + 1) * cb - 1, Console.Palette[15]);
			ColumnX[x] := xx;
			Inc(xx, ColumnWidth[x] + 1);
		end;
	end;

	for i := 0 to Items.Count-Offset-1 do
	begin
		y := Rect.Top + i;
		if y >= Rect.Bottom then Break;

		Assert(i + Offset < Items.Count,
			Format('TCWEList.Paint: Item index past bounds!#10#13Index=%d Offset=%d Items=%d',
			[i+Offset, Offset, Items.Count]));

		Item := Items[i + Offset];
		if Item = nil then Continue;

		// write list item text
		if Item.ColorBack >= 0 then
			cb := Item.ColorBack
		else
			cb := ColorBack;

		if Item.Data = LISTITEM_HEADER then
		begin
			if Item.ColorFore >= 0 then
				cf := Item.ColorFore
			else
				cf := ColorFore;
			Console.WriteHeader(Copy(Item.Captions[0], 1, Width+1), y, cf, cb,
				Rect.Left, Rect.Right);
		end
		else
		begin
			xx := Rect.Left;
			for x := 0 to Columns-1 do
			begin
				if (x = 0) and (Item.ColorFore >= 0) then
					cf := Item.ColorFore
				else
				if ColumnColor[x] < 0 then
					cf := ColorFore
				else
					cf := ColumnColor[x];

				if (x = 0) and (Item.Captions[1] = '') then
					Console.Write(Copy(Item.Captions[x], 1, Width+1),
						xx, y, cf, cb)
				else
					Console.Write(Copy(Item.Captions[x], 1,
						ColumnWidth[x]), xx, y, cf, cb);
				Inc(xx, ColumnWidth[x] + 1);
			end;
		end;

		if Assigned(FOnPaintItem) then
			FOnPaintItem(Self, Item, i + Offset, Types.Point(Rect.Left, y));

		// highlight active line
		if (i + Offset = ItemIndex) then
		begin
			if (Selection3D and (Hovered or Focused)) then
			begin
				if Item.Data = LISTITEM_HEADER then
					cf := -1 // don't highlight text
				else
				begin
					cf := Data[0].Value;
					SR := Types.Rect(Rect.Left, y, Rect.Right, y+1);
				end;

				for x := Rect.Left to Rect.Right-1 do
					Console.SetColor(x, y, cf, Data[1].Value);
			end
			else
			if not Selection3D then
			begin
				if Focused then
					xx := Data[0].Value // selected foreground color
				else
					xx := Data[4].Value; // unfocused sel. foreground color
				if xx <> 255 then
					cf := xx
				else
					cf := -1;

				if Focused then
					xx := Data[1].Value // selected background color
				else
					xx := Data[5].Value; // unfocused sel. bg color
				if xx <> 255 then
					cb := xx
				else
					cb := -1;

				for x := Rect.Left to Rect.Right-1 do
					Console.SetColor(x, y, cf, cb);
			end;
		end;
	end;

	// paint 3d border on selection
	if (Selection3D) and (SR.Left >= 0) and (Data[2].Value < 16) then
		Console.FrameRectPx(SR, False, False, -1, Data[2].Value, Data[3].Value);

	// dumb fix for last column text overflowing onto the scrollbar area
	if Scrollbar <> nil then
		Scrollbar.Paint;
end;

// ==========================================================================
{ TCWETwoColumnList }
// ==========================================================================

constructor TCWETwoColumnList.Create;
begin
	inherited;
	Columns := 2;
end;

// ==========================================================================
{ TCWEListItem }
// ==========================================================================

constructor TCWEListItem.Create(const Text: String;
	DataVal: Cardinal = 0; DataPtr: Pointer = nil;
	colFg: ShortInt = -1; colBg: ShortInt = -1);
var
	x: Integer;
	arr: TArrayOfString;
begin
	if Pos(COLUMNSEPARATOR, Text) > 0 then
	begin
		arr := SplitString(Text, COLUMNSEPARATOR);
		for x := 0 to High(arr) do
			if x <= MAXCOLUMNS then
				Captions[x] := arr[x];
	end
	else
		Captions[0] := Text;

	Data    := DataVal;
	ObjData := DataPtr;

	ColorFore := colFg;
	ColorBack := colBg;

	if DataVal = LISTITEM_HEADER then
	begin
		if colFg < 0 then
			ColorFore := 6;
		if colBg < 0 then
			ColorBack := 15;
	end;
end;

// ==========================================================================
{ TCWEMemo }
// ==========================================================================

constructor TCWEMemo.Create;
begin
	inherited;

	Lines := TStringList.Create;
	Lines.OwnsObjects := False;
	ColorFore := TConsole.COLOR_PANEL;

	WantMouse := True;
	WantKeyboard := True;
	WantHover := False;

	CreateScrollbar;
end;

destructor TCWEMemo.Destroy;
begin
	Lines.Free;
	inherited;
end;

procedure TCWEMemo.AdjustScrollbar;
begin
	MaxScroll := Max(Lines.Count - Height - 1, 0);
	if Assigned(Scrollbar) then
		Scrollbar.Adjust(Lines.Count-1, Height);
end;

procedure TCWEMemo.Add(const S: AnsiString; Color: ShortInt = -1; Center: Boolean = False);
var
	i: Integer;
begin
	if Color < 0 then Color := ColorFore;

	if Center then
		i := Lines.AddObject(StringOfChar(' ',
			(Rect.Right-Rect.Left) div 2 - (Length(S) div 2)) + S, TObject(Color))
	else
		i := Lines.AddObject(S, TObject(Color));

	if i >= (Offset + Height) then
		Offset := Max(i - Height + 0, 0); // +1->0 2016-12-01
	AdjustScrollbar;
end;

function TCWEMemo.KeyDown;
var
	Sc: ControlKeyNames;
begin
	inherited;

	Result := True;

	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, Shift));

	case Sc of

		ctrlkeyHOME:
			Offset := 0;

		ctrlkeyEND:
			Offset := Max(0, Lines.Count - Height - 1);

		ctrlkeyUP:
			if Offset > 0 then
				Dec(Offset);

		ctrlkeyDOWN:
			if Offset < (Lines.Count - Height - 1) then
				Inc(Offset);

		ctrlkeyPGUP:
			Offset := Max(0, Integer(Offset - Height));

		ctrlkeyPGDN:
			Offset := Min(Max(Lines.Count - Height - 1, 0), Offset + Height);

	else
		Exit(False);
	end;

	AdjustScrollbar;
	Paint;
end;

function TCWEMemo.MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean;
const
	ScrollAmount = 2;
begin
	Result := True;

	if Lines.Count < Height then
	begin
		Offset := 0;
		Exit;
	end;

	if WheelDelta < 0 then
		Offset := Min(Offset + ScrollAmount, Lines.Count - Height - 1)
	else
		Offset := Max(Integer(Offset - ScrollAmount), 0);

	AdjustScrollbar;
	Paint;
end;

function TCWEMemo.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
var
	S, W: String;
	X1, X2: Integer;
begin
	inherited;
	Result := False;

	X := P.X + 1;
	Y := P.Y + Offset;

	if (Button <> mbLeft) or (Y < 0) or (Y >= Lines.Count-1) then Exit;

	S := Lines[Y];
	if (X > Length(S)) or (S[X] = ' ') then Exit;

	X1 := X; // find start of word
	while (X1 > 0) and (S[X1] <> ' ') do Dec(X1);
	X2 := X; // find end of word
	while (X2 <= Length(S)) and (S[X2] <> ' ') do Inc(X2);

	W := Trim(Copy(S, X1+1, X2-X1));

	// if it's a hyperlink, open it
	{$IFDEF WINDOWS}
	if Pos('://', W) > 3 then
		ShellExecute(0, 'open', PChar(W), nil, nil, SW_SHOW);
	{$ENDIF}

	Result := True;
end;

procedure TCWEMemo.Paint;
var
	y, c: Integer;
	S: String;
begin
	if Border.Pixel then
	begin
		Console.FillRect(AdjustedRect, ' ', -1, TConsole.COLOR_BLANK);
		Console.FrameRectPx(AdjustedRect, Border.Sunken, Border.Fat);
	end
	else
		Console.FrameRect(AdjustedRect, Border.Sunken, Border.Fat, TConsole.COLOR_BLANK);

	inherited; // paint scrollbar

	for y := 0 to Height do
	begin
		if (y + Offset) >= Lines.Count then
			Break;
		S := Copy(Lines[y + Offset], 1, Width+1);
		if Lines.Objects[y + Offset] <> nil then
			c := Integer(Lines.Objects[y + Offset])
		else
			c := 3;
		Console.Write(S, Rect.Left, Rect.Top + y, c);
	end;
end;

// ==========================================================================
{ TCWEEdit }
// ==========================================================================

constructor TCWEEdit.Create;
begin
	inherited;

	WantMouse := True;
	WantKeyboard := True;
	WantHover := False;

	ReportAnyChange := False;
	AllowedChars := '';

	ColorBack := 0;
	ColorFore := 5;
	MaxLength := High(Word);

	SetData(0, TConsole.COLOR_LIGHT, 'Focused text');
	SetData(1, TConsole.COLOR_TEXT,  'Focused background');

	Offset := 0;
	Cursor.X := -1;
end;

function TCWEEdit.KeyDown;
var
	AtEnd: Boolean;
	Sc: ControlKeyNames;
begin
	AtEnd := (Cursor.X >= Length(Caption));
	Result := True;

	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, Shift));

	if (ssCtrl in Shift) and (Key = SDLK_BACKSPACE) then
	begin
		Caption := '';
		Cursor.X := 0;
	end
	else
	case Sc of

		ctrlkeyLEFT:
			Cursor.X := Max(Cursor.X-1, 0);

		ctrlkeyRIGHT:
			Cursor.X := Min(Cursor.X+1, Length(Caption));

		ctrlkeyPreviousCtrl,
		ctrlkeyUP:
			Screen.BrowseControls(True);

		ctrlkeyNextCtrl,
		ctrlkeyDOWN:
			Screen.BrowseControls(False);

		ctrlkeyBACKSPACE:
		begin
			if AtEnd then
				Caption := Copy(Caption, 1, Length(Caption)-1)
			else
				Delete(Caption, Cursor.X, 1);
			Cursor.X := Max(Cursor.X-1, 0);
			Change(ReportAnyChange);
		end;

		ctrlkeyDELETE:
			if not AtEnd then
			begin
				Delete(Caption, Cursor.X+1, 1);
				Change(ReportAnyChange);
			end;

		ctrlkeyHOME:
			Cursor.X := 0;

		ctrlkeyEND:
			Cursor.X := Length(Caption);

		ctrlkeyRETURN:
			Change(True); // trigger change notify event

	else
		begin
			{if not (Key in [32..127]) then
				Key := 0;} // ???
			Exit(False);
		end;
	end;

	Key := 0;
	Paint;
end;

function TCWEEdit.TextInput(var Key: Char): Boolean;
begin
	Result := False;
	if (Ord(Key) < 32) or (ModKeys(ssCtrl)) then Exit;
	if (AllowedChars <> '') and (Pos(Key, AllowedChars) < 1) then Exit;
	if Length(Caption) >= 255 then Exit;

	Insert(Key, Caption, Cursor.X+1);
	Caption := Copy(Caption, 1, MaxLength);
	Inc(Cursor.X);

	Result := True;
	Change(ReportAnyChange);
end;

function TCWEEdit.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if Button <> mbLeft then Exit(False);

	Result := True;
	Cursor.X := Min(P.X + Offset, Length(Caption));
	Paint;
end;

procedure TCWEEdit.Paint;
var
	C, B: Byte;
begin
	if not Screen.Active then Exit;

	DrawBorder;

	if Cursor.X >= MaxLength then
		Cursor.X := MaxLength-1;

	if Cursor.X < 0 then
		Cursor.X := Length(Caption)
	else
	if Cursor.X < Offset then
		Offset := Cursor.X
	else
	if Cursor.X > (Offset + Width + 0) then
		Offset := Max(0, Cursor.X - Width - 0);

	if (Focused) or (Hovered) then
	begin
		C := Data[0].Value;
		B := Data[1].Value;
	end
	else
	begin
		C := ColorFore;
		B := ColorBack;
	end;

	Console.FillRect(Rect, ' ', C, B);
	Console.Write(Copy(Caption, Offset + 1, Rect.Right-Rect.Left), Rect.Left, Rect.Top);

	if Focused then
	begin
		C := Rect.Left + Cursor.X - Offset;
		if C < Rect.Right then
			Console.SetColor(C, Cursor.Y + Rect.Top, TConsole.COLOR_TEXT, TConsole.COLOR_LIGHT);
	end;
end;

procedure TCWEEdit.SetCaption(const NewCaption: AnsiString; CursorAtStart: Boolean = False);
begin
	Caption := Copy(NewCaption, 1, MaxLength);
	Offset := 0;
	if CursorAtStart then
		Cursor.X := 0
	else
		Cursor.X := Length(Caption);
	Paint;
end;

// ==========================================================================
{ TCWEScreenHelper }
// ==========================================================================

function TCWEScreenHelper.GenerateHeaderText(const KeyName: AnsiString = ''): AnsiString;
begin
	if KeyName = '' then
		Result := ''
	else
		Result := Format('%s (%s)', [Caption, ShortCuts.GetShortcut('', KeyName)]);
end;

function TCWEScreenHelper.AddHeader(const KeyName: AnsiString = ''): TCWELabel;
begin
	Result := TCWELabel.Create(Self, GenerateHeaderText(KeyName),
		'Header', Types.Rect(1, 1, 79, 2));
	Result.Alignment := ALIGN_HEADER;
	GenerateHeaderText;
end;

function TCWEScreenHelper.UpdateHeader(const KeyName: AnsiString = ''): TCWELabel;
var
	Ctrl: TCWEControl;
begin
	Result := nil;
	Ctrl := Self.FindControl('Header');
	if (Ctrl is TCWELabel) then
	begin
		Result := Ctrl as TCWELabel;
		Result.SetCaption(GenerateHeaderText(KeyName));
	end;
end;

end.
