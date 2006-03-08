(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{** INTERFACE *************************************}{$IFDEF INC_READ_INTERFACE}

  TTreeModel = class(TComponent)
  private
    FStructure: String;
    procedure SetStructure(AValue: String);
  protected
    Handle: Pointer;
    //==
    procedure CreateHandle; virtual; abstract;
    procedure DestroyHandle; virtual;
    procedure RecreateHandle; inline;
    procedure SetValue(const AIter: TTreeIter; ACol: Integer; AGValue: Pointer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //--
    function GetIterFirst(var AIter: TTreeIter): Boolean;
    function GetIterFromPath(APath: String; var AIter: TTreeIter): Boolean;
    function GetPath(const AIter: TTreeIter): String;
    function IterNext(var AIter: TTreeIter): Boolean;
    function IterChildren(var AIter: TTreeIter; const AParent: TTreeIter): Boolean;
    function IterHasChild(const AIter: TTreeIter): Boolean;
    function IterNChildren(const AIter: TTreeIter): Integer;
    function IterNthChild(var AIter: TTreeIter; const AParent: TTreeIter; AN: Integer): Boolean;
    function IterParent(var AIter: TTreeIter; const AChild: TTreeIter): Boolean;
    //== Values
    function GetIntegerValue(const AIter: TTreeIter; ACol: Integer): Integer;
    procedure SetIntegerValue(const AIter: TTreeIter; ACol: Integer; AValue: Integer);
    function GetStringValue(const AIter: TTreeIter; ACol: Integer): String;
    procedure SetStringValue(const AIter: TTreeIter; ACol: Integer; AValue: String);
    function GetPointerValue(const AIter: TTreeIter; ACol: Integer): Pointer;
    procedure SetPointerValue(const AIter: TTreeIter; ACol: Integer; AValue: Pointer);
  published
    property Structure: String read FStructure write SetStructure;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TTreeModel }

constructor TTreeModel.Create(AOwner: TComponent);
begin
  FStructure := 'S';
  inherited;
  CreateHandle;
  g_object_ref(Handle);
end;

destructor TTreeModel.Destroy;
begin
  DestroyHandle;
  g_object_unref(Handle);
  inherited;
end;

procedure TTreeModel.DestroyHandle;
begin
end;

procedure TTreeModel.RecreateHandle; inline;
begin
  DestroyHandle;
  g_object_unref(Handle);
  // TODO: Avisar os TreeViews

  CreateHandle;
  g_object_ref(Handle);
end;

procedure TTreeModel.SetStructure(AValue: String);
begin
  AValue := UpperCase(AValue);
  if (AValue <> FStructure) and (Length(FStructure) > 0) then
  begin
    FStructure := AValue;
    RecreateHandle;
  end;
end;

function TTreeModel.GetIterFromPath(APath: String; var AIter: TTreeIter): Boolean;
begin
  Result := gtk_tree_model_get_iter_from_string(Handle, @AIter, PChar(APath));
end;

function TTreeModel.GetIterFirst(var AIter: TTreeIter): Boolean;
begin
  Result := gtk_tree_model_get_iter_first(Handle, @AIter);
end;

function TTreeModel.GetPath(const AIter: TTreeIter): String;
var
  P: PGtkTreePath;
  S: PChar;
begin
  P := gtk_tree_model_get_path(Handle, @AIter);
  S := gtk_tree_path_to_string(P);
  Result := S;
  g_free(S);
  gtk_tree_path_free(P);
end;

function TTreeModel.IterNext(var AIter: TTreeIter): Boolean;
begin
  Result := gtk_tree_model_iter_next(Handle, @AIter);
end;

function TTreeModel.IterChildren(var AIter: TTreeIter; const AParent: TTreeIter): Boolean;
begin
  Result := gtk_tree_model_iter_children(Handle, @AIter, @AParent);
end;

function TTreeModel.IterHasChild(const AIter: TTreeIter): Boolean;
begin
  Result := gtk_tree_model_iter_has_child(Handle, @AIter);
end;

function TTreeModel.IterNChildren(const AIter: TTreeIter): Integer;
begin
  Result := gtk_tree_model_iter_n_children(Handle, @AIter);
end;

function TTreeModel.IterNthChild(var AIter: TTreeIter; const AParent: TTreeIter; AN: Integer): Boolean;
begin
  Result := gtk_tree_model_iter_nth_child(Handle, @AIter, @AParent, AN);
end;

function TTreeModel.IterParent(var AIter: TTreeIter; const AChild: TTreeIter): Boolean;
begin
  Result := gtk_tree_model_iter_parent(Handle, @AIter, @AChild);
end;

function TTreeModel.GetStringValue(const AIter: TTreeIter; ACol: Integer): String;
var
  V: TGValue;
begin
  if FStructure[ACol+1] <> 'S' then
    raise Exception.CreateFmt('Column %d is not a String', [ACol]);

  V.g_type := 0;
  gtk_tree_model_get_value(Handle, @AIter, ACol, @V);
  Result := g_value_get_string(@V);
  g_value_unset(@V);
end;

procedure TTreeModel.SetStringValue(const AIter: TTreeIter; ACol: Integer; AValue: String);
var
  V: TGValue;
begin
  if FStructure[ACol+1] <> 'S' then
    raise Exception.CreateFmt('Column %d is not a String', [ACol]);

  V.g_type := 0;
  g_value_init(@V, G_TYPE_STRING);
  g_value_set_string(@V, PChar(AValue));
  SetValue(AIter, ACol, @V);
  g_value_unset(@V);
end;

function TTreeModel.GetPointerValue(const AIter: TTreeIter; ACol: Integer): Pointer;
var
  V: TGValue;
begin
  if FStructure[ACol+1] <> 'P' then
    raise Exception.CreateFmt('Column %d is not a Pointer', [ACol]);

  V.g_type := 0;
  gtk_tree_model_get_value(Handle, @AIter, ACol, @V);
  Result := g_value_get_pointer(@V);
  g_value_unset(@V);
end;

procedure TTreeModel.SetPointerValue(const AIter: TTreeIter; ACol: Integer; AValue: Pointer);
var
  V: TGValue;
begin
  if FStructure[ACol+1] <> 'P' then
    raise Exception.CreateFmt('Column %d is not a Pointer', [ACol]);
  V.g_type := 0;
  g_value_init(@V, G_TYPE_POINTER);
  g_value_set_pointer(@V, AValue);
  SetValue(AIter, ACol, @V);
  g_value_unset(@V);
end;

function TTreeModel.GetIntegerValue(const AIter: TTreeIter; ACol: Integer): Integer;
var
  V: TGValue;
begin
  if FStructure[ACol+1] <> 'I' then
    raise Exception.CreateFmt('Column %d is not a Integer', [ACol]);

  V.g_type := 0;
  gtk_tree_model_get_value(Handle, @AIter, ACol, @V);
  Result := g_value_get_int(@V);
  g_value_unset(@V);
end;

procedure TTreeModel.SetIntegerValue(const AIter: TTreeIter; ACol: Integer; AValue: Integer);
var
  V: TGValue;
begin
  if FStructure[ACol+1] <> 'I' then
    raise Exception.CreateFmt('Column %d is not a Integer', [ACol]);
  V.g_type := 0;
  g_value_init(@V, G_TYPE_INT);
  g_value_set_int(@V, AValue);
  SetValue(AIter, ACol, @V);
  g_value_unset(@V);
end;

{$ENDIF}
