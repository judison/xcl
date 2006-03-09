(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{** INTERFACE *************************************}{$IFDEF INC_READ_INTERFACE}

  TTreeStore = class(TTreeModel)
  {$DEFINE INTF_DECLARATION}
    {$I itreesortable.inc}
  {$UNDEF INTF_DECLARATION}
  private
  protected
    procedure CreateHandle; override;
    procedure SetValue(const AIter: TTreeIter; ACol: Integer; AGValue: Pointer); override;
  public
    procedure Remove(const AIter: TTreeIter);
    procedure Insert(var AIter: TTreeIter; APosition: Integer);
    procedure Insert(var AIter: TTreeIter; const AParent: TTreeIter; APosition: Integer);
    procedure InsertBefore(var AIter: TTreeIter; const ASibling: TTreeIter);
    procedure InsertAfter(var AIter: TTreeIter; const ASibling: TTreeIter);
    procedure Prepend(var AIter: TTreeIter);
    procedure Prepend(var AIter: TTreeIter; const AParent: TTreeIter);
    procedure Append(var AIter: TTreeIter);
    procedure Append(var AIter: TTreeIter; const AParent: TTreeIter);
    function IsAncestor(const AIter: TTreeIter; const ADescendant: TTreeIter): Boolean;
    function IterDepth(const AIter: TTreeIter): Integer;
    procedure Clear;
    procedure Swap(var A, B: TTreeIter);
    procedure MoveBefore(var AIter: TTreeIter; const APosition: TTreeIter);
    procedure MoveAfter(var AIter: TTreeIter; const APosition: TTreeIter);
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TTreeStore }

procedure TTreeStore.CreateHandle;
var
  X: array of GType;
  I: Integer;
begin
  SetLength(X, Length(FStructure));
  for I := 1 to Length(FStructure) do
    case FStructure[I] of
      'I': X[I-1] := G_TYPE_INT;
      'U': X[I-1] := G_TYPE_UINT;
      'S': X[I-1] := G_TYPE_STRING;
      'B': X[I-1] := G_TYPE_BOOLEAN;
      'P': X[I-1] := G_TYPE_POINTER;
    else
      raise Exception.Create('Invalid Structure');
    end;
  Handle := gtk_tree_store_newv(Length(FStructure), @X[0]);
  g_object_ref(Handle);
end;

procedure TTreeStore.SetValue(const AIter: TTreeIter; ACol: Integer; AGValue: Pointer);
begin
  gtk_tree_store_set_value(Handle, @AIter, ACol, AGValue);
end;

procedure TTreeStore.Remove(const AIter: TTreeIter);
begin
  //Result := gtk_tree_store_remove(Handle, @AIter);
  gtk_tree_store_remove(Handle, @AIter);
end;

procedure TTreeStore.Insert(var AIter: TTreeIter; APosition: Integer);
begin
  gtk_tree_store_insert(Handle, @AIter, nil, APosition);
end;

procedure TTreeStore.Insert(var AIter: TTreeIter; const AParent: TTreeIter; APosition: Integer);
begin
  gtk_tree_store_insert(Handle, @AIter, @AParent, APosition);
end;

procedure TTreeStore.InsertBefore(var AIter: TTreeIter; const ASibling: TTreeIter);
begin
  gtk_tree_store_insert_before(Handle, @AIter, nil, @ASibling);
end;

procedure TTreeStore.InsertAfter(var AIter: TTreeIter; const ASibling: TTreeIter);
begin
  gtk_tree_store_insert_after(Handle, @AIter, nil, @ASibling);
end;

procedure TTreeStore.Prepend(var AIter: TTreeIter);
begin
  gtk_tree_store_prepend(Handle, @AIter, nil);
end;

procedure TTreeStore.Prepend(var AIter: TTreeIter; const AParent: TTreeIter);
begin
  gtk_tree_store_prepend(Handle, @AIter, @AParent);
end;

procedure TTreeStore.Append(var AIter: TTreeIter);
begin
  gtk_tree_store_append(Handle, @AIter, nil);
end;

procedure TTreeStore.Append(var AIter: TTreeIter; const AParent: TTreeIter);
begin
  gtk_tree_store_append(Handle, @AIter, @AParent);
end;

function TTreeStore.IsAncestor(const AIter: TTreeIter; const ADescendant: TTreeIter): Boolean;
begin
  Result := gtk_tree_store_is_ancestor(Handle, @AIter, @ADescendant);
end;

function TTreeStore.IterDepth(const AIter: TTreeIter): Integer;
begin
  Result := gtk_tree_store_iter_depth(Handle, @AIter);
end;

procedure TTreeStore.Clear;
begin
  gtk_tree_store_clear(Handle);
end;

procedure TTreeStore.Swap(var A, B: TTreeIter); // var ou const ??? GTK muda os Iter's ?
begin
  gtk_tree_store_swap(Handle, @A, @B);
end;

procedure TTreeStore.MoveBefore(var AIter: TTreeIter; const APosition: TTreeIter);
begin
  gtk_tree_store_move_before(Handle, @AIter, @APosition);
end;

procedure TTreeStore.MoveAfter(var AIter: TTreeIter; const APosition: TTreeIter);
begin
  gtk_tree_store_move_after(Handle, @AIter, @APosition);
end;

{$DEFINE INTF_IMPLEMENTATION}
  {$define the_class := TTreeStore }
    {$I itreesortable.inc}
  {$undef the_class}
{$UNDEF INTF_IMPLEMENTATION}

{$ENDIF}
