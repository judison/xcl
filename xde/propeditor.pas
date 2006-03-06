(*
   $Id: propeditor.pas,v 1.4 2005/12/27 03:14:06 judison Exp $

   XDE - XCL's Development Environment
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit propeditor;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, xcl, typinfo;

type
  TComponentEditor = class(TComponent)
  private
    FComponent: TComponent;
    FPropEditors: TList;
    FPropTable: TTable;
    FEventTable: TTable;
    procedure SetComponent(AValue: TComponent);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Changed;
    //==
    property PropTable: TTable read FPropTable write FPropTable;
    property EventTable: TTable read FEventTable write FEventTable;
    property Component: TComponent read FComponent write SetComponent;
  end;

type
  TPropertyEditor = class(TBinControl)
  private
    FCompEditor: TComponentEditor;
  protected
    FProp: PPropInfo;
    FComp: TComponent;
    //==
    procedure CreateHandle; override;
    //==
    procedure Changed;                            // Alert CompEditor that my prop changed
    //==
    procedure CreateControls; virtual; abstract;  // Create my inner controls
    procedure CompChanged; virtual; abstract;     // Comp has changed, update my value
  public
    constructor Create(ACompEditor: TComponentEditor; AProp: PPropInfo); virtual; reintroduce;
    destructor Destroy; override;
  end;
  TPropertyEditorClass = class of TPropertyEditor;

  TUnknowPropEditor = class(TPropertyEditor)
  private
    FLabel: TLabel;
  protected
    procedure CreateControls; override;
    procedure CompChanged; override;
  public
  end;

  TStringPropEditor = class(TPropertyEditor)
  private
    FEntry: TEntry;
  protected
    procedure EntryFocusOut(Sender: TObject); virtual;
    procedure CreateControls; override;
    procedure CompChanged; override;
  public
  end;

  TIntegerPropEditor = class(TPropertyEditor)
  private
    FSpinButton: TSpinButton;
  protected
    procedure SpinButtonFocusOut(Sender: TObject); virtual;
    procedure CreateControls; override;
    procedure CompChanged; override;
  public
  end;

  TFloatPropEditor = class(TPropertyEditor)
  private
    FSpinButton: TSpinButton;
  protected
    procedure SpinButtonFocusOut(Sender: TObject); virtual;
    procedure CreateControls; override;
    procedure CompChanged; override;
  public
  end;

  TBooleanPropEditor = class(TPropertyEditor)
  private
    FLS: TListStore;
    FComboBox: TComboBox;
  protected
    procedure ComboBoxChanged(Sender: TObject); virtual;
    procedure CreateControls; override;
    procedure CompChanged; override;
  public
  end;

  TEnumPropEditor = class(TPropertyEditor)
  private
    FLS: TListStore;
    FComboBox: TComboBox;
  protected
    procedure ComboBoxChanged(Sender: TObject); virtual;
    procedure CreateControls; override;
    procedure CompChanged; override;
  public
  end;

  TComponentPropEditor = class(TPropertyEditor)
  private
    FLS: TListStore;
    FComboBox: TComboBox;
  protected
    procedure ComboBoxChanged(Sender: TObject); virtual;
    procedure CreateControls; override;
    procedure CompChanged; override;
  public
  end;

implementation

uses gtk2;

{ TComponentEditor }

constructor TComponentEditor.Create(AOwner: TComponent);
begin
  inherited;
  FPropEditors := TList.Create;
end;

destructor TComponentEditor.Destroy;
begin
  FPropEditors.Free;
  inherited;
end;

procedure TComponentEditor.Changed;
var
  I: Integer;
begin
  for I := 0 to FPropEditors.Count -1 do
    TPropertyEditor(FPropEditors[I]).CompChanged;
end;

procedure TComponentEditor.SetComponent(AValue: TComponent);
var
  PT: PTypeData;
  PP: PPropList;
  PI: PTypeInfo;
  I: Longint;
  TopP: Integer;
  C: TControl;
  O: TObject;
  procedure AddProp(AProp: PPropInfo; AClass: TPropertyEditorClass; AEvent: Boolean);
  var
    Lbl: TLabel;
    PropEd: TPropertyEditor;
  begin
    if (not (FComponent is TControl)) or (TControl(FComponent).DesignShowProp(AProp^.Name)) then
    begin
      Lbl := TLabel.Create(Self);
      if AEvent then
        Lbl.Parent := FEventTable
      else
        Lbl.Parent := FPropTable;
      Lbl.TableTopAttach := TopP;
      Lbl.TableLeftAttach := 1;
      Lbl.Caption := ' '+AProp^.Name;
      Lbl.YPad := 5;
      Lbl.TableXOptions := [aoFill];
      Lbl.TableYOptions := [aoFill];
      Lbl.SetAlignment(0, 0.5);


      PropEd := AClass.Create(Self, AProp);
      if AEvent then
        PropEd.Parent := FEventTable
      else
        PropEd.Parent := FPropTable;
      PropEd.TableTopAttach := TopP;
      PropEd.TableLeftAttach := 2;
      PropEd.TableXOptions := [aoFill, aoExpand, aoShrink];
      PropEd.TableYOptions := [aoFill];
      PropEd.WidthRequest := 50;
      FPropEditors.Add(PropEd);

      Inc(TopP);
    end;
  end;
begin
  PropTable.Hide;
  EventTable.Hide;
  try
    while FPropEditors.Count > 0 do
    begin
      C := TControl(FPropEditors[0]);
      FPropEditors.Delete(0);
      C.Parent := nil;
      C.Free;
    end;
  
    while PropTable.ControlCount > 0 do
    begin
      C := PropTable.Controls[0];
      C.Parent := nil;
      C.Free;
    end;
  
    while EventTable.ControlCount > 0 do
    begin
      C := EventTable.Controls[0];
      C.Parent := nil;
      C.Free;
    end;
  
    FComponent := AValue;
    //==
    PI := FComponent.ClassInfo;
    PT := GetTypeData(PI);
    GetMem(PP, PT^.PropCount * SizeOf(Pointer));
    GetPropInfos(PI, PP);
  
    TopP := 0;
  
    for I := 0 to PT^.PropCount -1 do
      case PP^[I]^.PropType^.Kind of
        tkEnumeration : AddProp(PP^[I], TEnumPropEditor, False);
        tkBool        : AddProp(PP^[I], TBooleanPropEditor, False);
        tkSString,
        tkLString,
        tkAString,
        tkWString     : AddProp(PP^[I], TStringPropEditor, False);
        tkInteger     : AddProp(PP^[I], TIntegerPropEditor, False);
        //tkInt64       : 
        tkFloat       : AddProp(PP^[I], TFloatPropEditor, False);
        //tkVariant     : 
        tkClass,
        tkObject      :
        begin
          O := GetObjectProp(FComponent, PP^[I]);
          if (O = nil) or (O is TComponent) then
            AddProp(PP^[I], TComponentPropEditor, False)
          else
            AddProp(PP^[I], TUnknowPropEditor, False); // Persistent
        end;
        tkMethod      : AddProp(PP^[I], TUnknowPropEditor, True);
      else
        AddProp(PP^[I], TUnknowPropEditor, False);
      end;
    Changed;
  finally
    PropTable.Show;
    EventTable.Show;
  end;
end;

{ TPropertyEditor }

constructor TPropertyEditor.Create(ACompEditor: TComponentEditor; AProp: PPropInfo);
begin
  FCompEditor := ACompEditor;
  FProp := AProp;
  FComp := FCompEditor.Component;
  inherited Create(ACompEditor);
end;

destructor TPropertyEditor.Destroy;
begin
  inherited;
end;

procedure TPropertyEditor.CreateHandle;
begin
  Handle := gtk_frame_new(nil);
  gtk_frame_set_shadow_type(Handle, GTK_SHADOW_NONE);
  //==
  CreateControls;
end;

procedure TPropertyEditor.Changed;
begin
  FCompEditor.Changed;
end;

{ TUnknowPropEditor }

procedure TUnknowPropEditor.CreateControls;
begin
  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Caption := 'Unknow Property';
end;

procedure TUnknowPropEditor.CompChanged;
var
  C: TObject;
begin
  case FProp^.PropType^.Kind of
    tkEnumeration : FLabel.Caption := GetEnumProp(FComp, FProp);
    tkBool        : if GetOrdProp(FComp, FProp) = 0 then FLabel.Caption := 'False' else FLabel.Caption := 'True';
    tkSString,
    tkLString,
    tkAString,
    tkWString     : FLabel.Caption := GetStrProp(FComp, FProp);
    tkInteger     : FLabel.Caption := IntToStr(GetInt64Prop(FComp, FProp));
    tkInt64       : FLabel.Caption := IntToStr(GetInt64Prop(FComp, FProp));
    tkFloat       : FLabel.Caption := FloatToStr(GetFloatProp(FComp, FProp));
    tkVariant     : FLabel.Caption := 'Variant'; // GetVariantProp(FComponent, AProp);
    tkClass,
    tkObject	  :
    begin
      C := GetObjectProp(FComp, FProp);
      if C is TComponent then
        FLabel.Caption := TComponent(C).Name + ': '+C.ClassName
      else
        FLabel.Caption := 'Non TComponent'
    end;
    tkMethod      :  FLabel.Caption := ' ?? Method ?? '; //GetMethodProp(FComponent, AProp);
  else
    FLabel.Caption := 'Unknow Property';
  end;
end;

{ TStringPropEditor }

procedure TStringPropEditor.CreateControls;
begin
  FEntry := TEntry.Create(Self);
  FEntry.Parent := Self;
  FEntry.OnFocusOut := @EntryFocusOut;
end;

procedure TStringPropEditor.CompChanged;
begin
  FEntry.Text := GetStrProp(FComp, FProp);
end;

procedure TStringPropEditor.EntryFocusOut(Sender: TObject);
begin
  SetStrProp(FComp, FProp, FEntry.Text);
  Changed;
end;

{ TIntegerPropEditor }

procedure TIntegerPropEditor.CreateControls;
begin
  FSpinButton := TSpinButton.Create(Self);
  FSpinButton.Parent := Self;
  FSpinButton.Min := Low(Integer);
  FSpinButton.Max := High(Integer);
  FSpinButton.OnFocusOut := @SpinButtonFocusOut;
end;

procedure TIntegerPropEditor.CompChanged;
begin
  FSpinButton.Value := GetOrdProp(FComp, FProp);
end;

procedure TIntegerPropEditor.SpinButtonFocusOut(Sender: TObject);
var
  I: Integer;
begin
  try
    I := Trunc(FSpinButton.Value);
    SetOrdProp(FComp, FProp, I);
    Changed;
  except
    CompChanged;
  end;
end;

{ TFloatPropEditor }

procedure TFloatPropEditor.CreateControls;
begin
  FSpinButton := TSpinButton.Create(Self);
  FSpinButton.Parent := Self;
//  FSpinButton.Min := [0; // ???
//  FSpinButton.Max := +200; // ??????
  FSpinButton.Digits := 3;
  FSpinButton.OnFocusOut := @SpinButtonFocusOut;
end;

procedure TFloatPropEditor.CompChanged;
begin
  FSpinButton.Value := GetFloatProp(FComp, FProp);
end;

procedure TFloatPropEditor.SpinButtonFocusOut(Sender: TObject);
begin
  try
    SetFloatProp(FComp, FProp, FSpinButton.Value);
    Changed;
  except
    CompChanged;
  end;
end;

{ TBooleanPropEditor }

procedure TBooleanPropEditor.CreateControls;
var
  It: TTreeIter;
begin
  FLS := TListStore.Create(Self);
  FLS.Structure := 'SI';

  FLS.Append(It);
  FLS.SetStringValue(It, 0, 'False');
  FLS.SetIntegerValue(It, 1, 0);

  FLS.Append(It);
  FLS.SetStringValue(It, 0, 'True');
  FLS.SetIntegerValue(It, 1, 1);

  FComboBox := TComboBox.Create(Self);
  FComboBox.Parent := Self;
  FComboBox.Model := FLS;
  FComboBox.TextColumn := 0;
  FComboBox.OnChanged := @ComboBoxChanged;
end;

procedure TBooleanPropEditor.CompChanged;
var
  b: integer;
  It: TTreeIter;
begin
  B := GetOrdProp(FComp, FProp);
  FLS.GetIterFromPath(IntToStr(B), It);
  FComboBox.SetActiveIter(It);
end;

procedure TBooleanPropEditor.ComboBoxChanged(Sender: TObject);
var
  It: TTreeIter;
begin
  if FComboBox.GetActiveIter(It) then
  begin
    SetOrdProp(FComp, FProp, FLS.GetIntegerValue(It, 1));
    Changed;
  end;
end;

{ TEnumPropEditor }

procedure TEnumPropEditor.CreateControls;
var
  It: TTreeIter;
  I: Integer;
  EnumType: PTypeInfo;
  S: String;
begin
  FLS := TListStore.Create(Self);
  FLS.Structure := 'SI';

  EnumType := FProp^.PropType;
  with GetTypeData(EnumType)^ do
    for I := MinValue to MaxValue do
    begin
      S := GetEnumName(EnumType, I);
      FLS.Append(It);
      FLS.SetStringValue(It, 0, S);
      FLS.SetIntegerValue(It, 1, I);
    end;

  FComboBox := TComboBox.Create(Self);
  FComboBox.Parent := Self;
  FComboBox.Model := FLS;
  FComboBox.TextColumn := 0;
  FComboBox.OnChanged := @ComboBoxChanged;
end;

procedure TEnumPropEditor.CompChanged;
var
  b: integer;
  It: TTreeIter;
begin
  B := GetOrdProp(FComp, FProp);
  FLS.GetIterFromPath(IntToStr(B), It);
  FComboBox.SetActiveIter(It);
end;

procedure TEnumPropEditor.ComboBoxChanged(Sender: TObject);
var
  It: TTreeIter;
begin
  if FComboBox.GetActiveIter(It) then
  begin
    SetOrdProp(FComp, FProp, FLS.GetIntegerValue(It, 1));
    Changed;
  end;
end;

{ TComponentPropEditor }

procedure TComponentPropEditor.CreateControls;
var
  It: TTreeIter;
  I: Integer;
  TheClass: TComponentClass;
  S: String;
  Ow: TComponent;
  C: TComponent;
begin
  FLS := TListStore.Create(Self);
  FLS.Structure := 'SI';

  TheClass := TComponentClass(GetObjectPropClass(FComp, FProp^.Name));

  FLS.Append(It);
  FLS.SetStringValue(It, 0, 'nil');
  FLS.SetIntegerValue(It, 1, 0);

  Ow := FComp.Owner;
  for I := 0 to Ow.ComponentCount-1 do
  begin
    C := Ow.Components[I];
    if (C is TheClass) and (C.Name <> '')then
    begin
      S := C.Name;
      FLS.Append(It);
      FLS.SetStringValue(It, 0, S);
      FLS.SetIntegerValue(It, 1, Integer(C));
    end;
  end;

  FComboBox := TComboBox.Create(Self);
  FComboBox.Parent := Self;
  FComboBox.Model := FLS;
  FComboBox.TextColumn := 0;
  FComboBox.OnChanged := @ComboBoxChanged;
end;

procedure TComponentPropEditor.CompChanged;
var
  B: Boolean;
  It: TTreeIter;
  C, PC: TComponent;
begin
  PC := TComponent(GetObjectProp(FComp, FProp));
  B := FLS.GetIterFirst(It);
  while B do
  begin
    C := TComponent(Pointer(FLS.GetIntegerValue(It, 1)));
    if C = PC then
    begin
      B := False;
      FComboBox.SetActiveIter(It);
    end
    else
      B := FLS.IterNext(It);
  end;
end;

procedure TComponentPropEditor.ComboBoxChanged(Sender: TObject);
var
  It: TTreeIter;
begin
  if FComboBox.GetActiveIter(It) then
  begin
    SetObjectProp(FComp, FProp, TComponent(Pointer(FLS.GetIntegerValue(It, 1))));
    Changed;
  end;
end;


end.