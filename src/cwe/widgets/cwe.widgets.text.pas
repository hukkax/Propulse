//
// ConsoleWidgetEngine: textual widgets
// by hukka 2016
//
unit CWE.Widgets.Text;

interface

uses
	Classes, Types, SysUtils, ShortcutManager,
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

	COLOR_LINK           = 10;
	COLOR_LINK_HOVER     = 11;
	COLOR_LINK_HOVERBACK = 15;

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
	private
		FOnClipboardCopy:  TCWENotifyEvent;
		FOnClipboardPaste: TCWENotifyEvent;
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
	private
		FIsDown:		Boolean;
	public
		Tag:			Integer;
		ModalResult:	SmallInt;
		Shortcut:		ShortcutManager.TShortCut;
		Toggle:			Boolean;

		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;

		procedure	DoToggle;
		procedure	Toggled(B: Boolean);
		procedure 	Paint; override;

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
		property	Down: Boolean read FIsDown write Toggled;
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
		CanCloseDialog:	Boolean;

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
		destructor  Destroy; override;

		procedure 	Clear;
		function	IsValidItemIndex(i: Integer): Boolean;
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

	TCWEMemo = class;
	TTextLine = class;

	TTextObjectKind = ( txtNormal, txtLink );

	TTextObject = class
	public
		X:			Word;
		Y:			Cardinal;
		ColorFore,
		ColorBack:  ShortInt;
		Kind:		TTextObjectKind;
		Line:		TTextLine;
		Text,
		Data:		AnsiString;

		constructor Create(const aLine: TTextLine; const aText: AnsiString;
					aX: Word; aY: Cardinal; colFg: ShortInt = -1; colBg: ShortInt = -1);

		procedure	Paint(DX, DY: Word);
		function 	OnHover: Boolean;
		function 	OnClick: Boolean;

		function 	Length: Word; inline;
		function 	X2: Word; inline;
		//function	GetPixelCoords: TRect;
	end;
	TTextObjectList = TObjectList<TTextObject>;

	TTextLine = class
	public
		Memo: 		TCWEMemo;
		Y:			Cardinal;
		Items:		TTextObjectList;

		constructor	Create(const aMemo: TCWEMemo; const S: AnsiString; aY: Cardinal;
					out Center, Box: Boolean);
		destructor	Destroy; override;

		function 	ObjectAt(X: Word): TTextObject;
		function	OnClick(X: Word): Boolean;
		function	GetText: AnsiString;
	end;
	TTextLineList = TObjectList<TTextLine>;

	TTextAnchor = class
	public
		Y:		Cardinal;
		Name:	AnsiString;

		constructor	Create(const S: AnsiString; aY: Cardinal);
	end;
	TAnchorList = TObjectList<TTextAnchor>;

	TCWEMemo = class(TCWEScrollableControl)
	private
		procedure 	AdjustScrollbar;
	public
		Lines:		TTextLineList;
		Anchors:	TAnchorList;
		HoveredTextObject: TTextObject;

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					IsProtected: Boolean = False); override;
		destructor  Destroy; override;

		procedure 	Add(const S: AnsiString; Color: ShortInt = -1; Center: Boolean = False); virtual;

		function 	GetSection(const AnchorName: AnsiString): TStringList;
		function	FindSection(const AnchorName: AnsiString): Integer;
		function	JumpToSection(const AnchorName: AnsiString): Integer;

		function 	KeyDown(var Key: Integer; Shift: TShiftState): Boolean; override;
		function	MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean; override;
		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		procedure 	MouseMove(X, Y: Integer; P: TPoint); override;

		procedure 	Paint; override;
	end;


implementation

uses
	{$IFDEF WINDOWS}Windows{$ELSE}Process{$ENDIF},
	MainWindow, DateUtils,
	CWE.ExternalAPI, CWE.Dialogs,
	TextMode, Math,
	ProTracker.Util;

// ==========================================================================
{ TTextAnchor }
// ==========================================================================

constructor TTextAnchor.Create(const S: AnsiString; aY: Cardinal);
begin
	inherited Create;
	Name := S;
	Y := aY;
end;

// ==========================================================================
{ TTextLine }
// ==========================================================================

constructor TTextLine.Create(const aMemo: TCWEMemo; const S: AnsiString; aY: Cardinal;
	out Center, Box: Boolean);
var
	DX: Integer;
	Txt: TTextObject;

	procedure AddItem(const S: AnsiString);
	begin
		Txt.Text := S;
		Items.Add(Txt);
		Inc(DX, Length(S));
		Txt := TTextObject.Create(Self, '', DX, Y, aMemo.ColorFore);
		Txt.Kind := txtNormal;
	end;

const
	ShortTags = '#@FB';
var
	x, t: Integer;
	Tag, RS: AnsiString;
	InTag: Boolean;
	C: AnsiChar;
begin
	inherited Create;

	Y := aY;
	DX := 0;
	Memo := aMemo;

	Center := False;
	Box := False;

	Items := TTextObjectList.Create;
	Txt := TTextObject.Create(Self, '', DX, Y, aMemo.ColorFore);

	if Pos('<', S) > 0 then
	begin
		RS := '';
		InTag := False;

		for x := 1 to Length(S) do
		begin
			C := S[x];

			if not InTag then
			begin
				if C = '<' then
				begin
					InTag := (Copy(S, x+1, 1) <> '<');
					if InTag then
					begin
						if RS <> '' then
						begin
							AddItem(RS);
							RS := '';
						end;
						Tag := '';
					end;
				end;
				if not InTag then
					RS := RS + C;
			end
			else
			begin
				if C = '>' then
				begin
					if Tag <> '' then
					begin
						C := UpperCase(Tag)[1];

						if Pos(C, ShortTags) > 0 then
						begin
							Tag := Copy(Tag, 2, MaxInt);

							if C = '#' then		// anchor (destination)
							begin
								Memo.Anchors.Add(TTextAnchor.Create(Tag, Y));
							end
							else
							if C = '@' then		// hyperlink
							begin
								t := Pos('=', Tag);
								if t > 0 then
								begin
									Txt.Data := Copy(Tag, 1, t-1);
									Tag := Copy(Tag, t+1, MaxInt);
								end
								else
									Txt.Data := Tag;

								Txt.ColorFore := COLOR_LINK;
								Txt.Kind := txtLink;

								AddItem(Tag);
							end
							else
							if C = 'F' then		// foreground color
							begin
								if Tag = '' then
									Txt.ColorFore := aMemo.ColorFore
								else
									Txt.ColorFore := StrToInt('$' + Tag);
							end
							else
							if C = 'B' then		// background color
							begin
								if Tag = '' then
									Txt.ColorBack := Memo.ColorBack
								else
									Txt.ColorBack := StrToInt('$' + Tag);
							end;
						end
						else
						begin
							Tag := UpperCase(Tag);

							if Tag = 'C' then	// center line
							begin
								Center := True;
							end
							else
							if Tag = 'H1' then	// center + white + box around line
							begin
								Center := True;
								Box := True;
								Txt.ColorFore := 11;
								Txt.ColorBack := 15;
							end
							else
							if Tag = 'H2' then	// center + white
							begin
								Center := True;
								Txt.ColorFore := 11;
							end
							else
							if Tag = 'H3' then	// red text
							begin
								Txt.ColorFore := 4;
							end
							else
							if Tag = 'HR' then	// horizontal line
							begin
								AddItem(' ' + StringOfChar(#154, Memo.Rect.Right-Memo.Rect.Left-2));
							end;

						end;

						Tag := '';
						InTag := False;
					end;
				end
				else
					Tag := Tag + C;

			end;

		end;

	end
	else
		RS := S;

	if RS <> '' then
	begin
		Txt.Text := RS;
		Items.Add(Txt);
	end
	else
	if Txt.Text = '' then
		Txt.Free;

	if Center then
	begin
		x := ((Memo.Rect.Right - Memo.Rect.Left) div 2) - (Length(GetText) div 2);
		for Txt in Items do
			Txt.X := Txt.X + x;
	end;
end;

destructor TTextLine.Destroy;
begin
	Items.Free;
	inherited Destroy;
end;

function TTextLine.ObjectAt(X: Word): TTextObject;
var
	Txt: TTextObject;
begin
	for Txt in Items do
		if (X >= Txt.X) and (X <= Txt.X2) then
			Exit(Txt);
	Result := nil;
end;

function TTextLine.OnClick(X: Word): Boolean;
var
	Txt: TTextObject;
begin
	Txt := ObjectAt(X);
	if Txt <> nil then
		Result := Txt.OnClick
	else
		Result := False;
end;

function TTextLine.GetText: AnsiString;
var
	Txt: TTextObject;
begin
	Result := '';
	for Txt in Items do
		Result := Result + Txt.Text;
end;

// ==========================================================================
{ TTextObject }
// ==========================================================================

constructor TTextObject.Create(const aLine: TTextLine; const aText: AnsiString;
	aX: Word; aY: Cardinal; colFg: ShortInt; colBg: ShortInt);
begin
	Line := aLine;
	Text := aText;
	X := aX;
	Y := aY;
	ColorFore := colFg;
	ColorBack := colBg;
end;

{function TTextObject.GetPixelCoords: TRect;
begin
end;}

function TTextObject.X2: Word;
begin
	Result := X + System.Length(Text);
end;

function TTextObject.Length: Word;
begin
	Result := System.Length(Text);
end;

function TTextObject.OnHover: Boolean;
begin
	if (Kind = txtLink) then
	begin
		Result := True;
		if Self <> Line.Memo.HoveredTextObject then
		begin
			Line.Memo.HoveredTextObject := Self;
			Line.Memo.Paint;
		end;
	end
	else
		Result := False;
end;

procedure OpenURL(const URL: String);
{$IFNDEF WINDOWS}
var
	Foo: String;
{$ENDIF}
begin
	{$IFDEF WINDOWS}
	ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOW);
	{$ELSE}
	RunCommand('xdg-open', [URL], Foo);
	{$ENDIF}
end;

function TTextObject.OnClick: Boolean;
begin
	if Kind = txtLink then
	begin
		if Pos('://', Data) > 3 then
		begin
			OpenURL(Data);
			Exit(True);
		end
		else
			Line.Memo.JumpToSection(Data);
	end;
	Result := False;
end;

procedure TTextObject.Paint(DX, DY: Word);
var
	c: ShortInt;
begin
	if Self = Line.Memo.HoveredTextObject then
		Console.Write(Text, DX + X, DY, COLOR_LINK_HOVER, COLOR_LINK_HOVERBACK)
	else
	begin
		c := ColorBack;
		if c < 0 then c := Line.Memo.ColorBack;
		Console.Write(Text, DX + X, DY, ColorFore, c);
	end;
end;

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

	Painted;
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

function TCWEURLLabel.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if Button = mbLeft then OpenURL(Caption);
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

	Toggle := False;
	FIsDown := False;

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
		ctrlkeyRETURN, ctrlkeySPACE:
		begin
			DoToggle;
			if Assigned(FOnChange) then
				FOnChange(Self);
		end;
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
		DoToggle;
		if Assigned(FOnChange) then
		begin
			Result := True;
			FOnChange(Self);
		end;
	end;
end;

procedure TCWEButton.DoToggle;
begin
	if Toggle then
	begin
		FIsDown := not FIsDown;
		Toggled(FIsDown);
	end;
end;

procedure TCWEButton.Toggled;
begin
	FIsDown := B;
	if (Width = 1) and (Height = 1) then
		if FIsDown then Caption := #251 else Caption := ' ';
	Paint;
end;

procedure TCWEButton.Paint;
var
	colF: Byte;
begin
	if not Screen.Active then Exit;

	inherited;

	colF := ColorFore;

	if (Focused) and (Hovered) and (Screen.MouseInfo.Buttons[mbLeft]) then // pressing down
	begin
		colF := TConsole.COLOR_3DLIGHT;
		Console.FrameRectPx(Rect, True);
	end
	else
	begin
		if (Hovered) {or (Focused)} then // mouse hovering
			colF := TConsole.COLOR_3DLIGHT;
		if (Focused and WantKeyboard) then
			Console.FrameRectPx(Rect, FIsDown, False{True})
		else
		begin
			// hack to remove the fat border from a previous draw op
			Console.FrameRectPx(Rect, FIsDown, True, -1, TConsole.COLOR_PANEL, TConsole.COLOR_PANEL);
			Console.FrameRectPx(Rect, FIsDown, False);
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

// ==========================================================================
{ TCWEList }
// ==========================================================================

constructor TCWEList.Create(Owner: TCWEControl; const sCaption,
	sID: AnsiString; const Bounds: TRect; IsProtected: Boolean);
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
	CanCloseDialog := False;

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

function TCWEList.IsValidItemIndex(i: Integer): Boolean;
begin
	Result := (Assigned(Items)) and (i >= 0) and (i < Items.Count);
end;

function TCWEList.GetCaption(index: Byte = 0): String;
begin
	if (Assigned(Items)) and (Items.Count > 0) and (ItemIndex >= 0) and (ItemIndex < Items.Count) then
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
				if (CanCloseDialog) and (InModalDialog) then
					ModalDialog.Dialog.Dismiss(True)
				else
				begin
					Key := KEY_RETURN;
					KeyDown(Key, []);
				end;
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

constructor TCWEMemo.Create(Owner: TCWEControl; const sCaption,
	sID: AnsiString; const Bounds: TRect; IsProtected: Boolean);
begin
	inherited;

	Lines := TTextLineList.Create;
	Anchors := TAnchorList.Create;

	HoveredTextObject := nil;

	ColorFore := TConsole.COLOR_PANEL;

	WantMouse := True;
	WantKeyboard := True;
	WantHover := False;

	CreateScrollbar;
end;

destructor TCWEMemo.Destroy;
begin
	Lines.Free;
	Anchors.Free;
	inherited;
end;

procedure TCWEMemo.AdjustScrollbar;
begin
	MaxScroll := Max(Lines.Count - Height - 1, 0);
	if Assigned(Scrollbar) then
		Scrollbar.Adjust(Lines.Count-1, Height);
end;

function TCWEMemo.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
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
			Offset := Min(Max(Lines.Count - Height - 1, 0), Integer(Offset + Height));

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

procedure TCWEMemo.MouseMove(X, Y: Integer; P: TPoint);
var
	Txt: TTextObject;
begin
	if (not Assigned(Screen)) then Exit;

	X := P.X + 1;
	Y := P.Y + Offset;

	if (Y < 0) or (Y >= Lines.Count-1) then Exit;

	Txt := Lines[Y].ObjectAt(X);
	if (Txt = nil) or (not Txt.OnHover) and (HoveredTextObject <> nil) then
	begin
		HoveredTextObject := nil;
		Paint;
	end;
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

	Result := Lines[Y].OnClick(X);
	if Result then Exit;

	S := Lines[Y].GetText;
	if (X > Length(S)) or (S[X] = ' ') then Exit;

	X1 := X; // find start of word
	while (X1 > 0) and (S[X1] <> ' ') do Dec(X1);
	X2 := X; // find end of word
	while (X2 <= Length(S)) and (S[X2] <> ' ') do Inc(X2);

	W := Trim(Copy(S, X1+1, X2-X1));

	// if it's a hyperlink, open it
	if Pos('://', W) > 3 then OpenURL(W);

	Result := True;
end;

procedure TCWEMemo.Add(const S: AnsiString; Color: ShortInt = -1; Center: Boolean = False);
var
	i: Integer;
	TS: AnsiString;
	Box: Boolean;
	LI: TTextLine;
begin
	TS := '';

	if Center then
		TS := '<C>';
	if Color >= 0 then
		TS := TS + '<F' + IntToHex(Color, 1) + '>';

	TS := TS + S;

	i := Pos('<H1>', UpperCase(TS));
	if i > 0 then
		TS := Copy(TS, 1, i+3) + #131 + ' ' + Copy(TS, i+4, MaxInt) + ' ' + #132;

	LI := TTextLine.Create(Self, TS, Lines.Count, Center, Box);

	if Box then
	begin
		i := Length(LI.GetText) - 2;

		Lines.Add(TTextLine.Create(Self, '<c><bF><fB>' + #128 + StringOfChar(#129, i) + #130,
			Lines.Count, Center, Box));

		Lines.Add(LI);

		Lines.Add(TTextLine.Create(Self, '<c><bF><fB>' + #133 + StringOfChar(#134, i) + #135,
			Lines.Count, Center, Box));
	end
	else
		Lines.Add(LI);

	i := Lines.Count;
	if i >= (Offset + Height) then
		Offset := Max(i - Height - 1, 0);

	AdjustScrollbar;
end;

function TCWEMemo.FindSection(const AnchorName: AnsiString): Integer;
var
	A: TTextAnchor;
begin
	for A in Anchors do
		if A.Name = AnchorName then
			Exit(A.Y);
	Result := -1;
end;

function TCWEMemo.JumpToSection(const AnchorName: AnsiString): Integer;
begin
	Result := FindSection(AnchorName);
	if Result >= 0 then
		ScrollTo(Result);
end;

function TCWEMemo.GetSection(const AnchorName: AnsiString): TStringList;
var
	A: TTextAnchor;
	i, y, line, last: Integer;
begin
	for i := 0 to Anchors.Count-1 do
	begin
		A := Anchors[i];
		if A.Name = AnchorName then
		begin
			y := A.Y;
			if i >= Anchors.Count-1 then
				last := Lines.Count-1
			else
				last := Anchors[i+1].Y-1;

			Result := TStringList.Create;
			for line := y to last do
				Result.Add(Lines[line].GetText);

			Exit;
		end;
	end;

	Result := nil;
end;

procedure TCWEMemo.Paint;
var
	y: Integer;
//	S: String;
	Txt: TTextObject;
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
//		S := Copy(Lines[y + Offset], 1, Width+1);
		for Txt in Lines[y+Offset].Items do
			Txt.Paint(Rect.Left, Rect.Top + y);

{		begin
			if Txt = HoveredTextObject then
				Console.Write(Txt.Text, Rect.Left + Txt.X, Rect.Top + y, 13)
			else
				Console.Write(Txt.Text, Rect.Left + Txt.X, Rect.Top + y, Max(Txt.ColorFore, 3));
		end;}
	end;
end;

// ==========================================================================
{ TCWEEdit }
// ==========================================================================

constructor TCWEEdit.Create(Owner: TCWEControl; const sCaption,
	sID: AnsiString; const Bounds: TRect; IsProtected: Boolean);
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

function TCWEEdit.KeyDown(var Key: Integer; Shift: TShiftState): Boolean;
var
	AtEnd: Boolean;
	Sc: ControlKeyNames;
begin
	AtEnd := (Cursor.X >= Length(Caption));
	Result := True;

	Sc := ControlKeyNames(Shortcuts.Find(ControlKeys, Key, Shift));

	if (ssCtrl in Shift) and (Key = KEY_BACKSPACE) then
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

		ctrlkeyCOPY:
			if Assigned(FOnClipboardCopy) then
				FOnClipboardCopy(Self)
			else
				ClipboardCopy(Self);

		ctrlkeyPASTE:
			if Assigned(FOnClipboardPaste) then
				FOnClipboardPaste(Self)
			else
				ClipboardPaste(Self);

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
	if (Ord(Key) < 32) {or (ModKeys(ssCtrl))} then Exit;
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
