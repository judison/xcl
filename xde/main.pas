(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit main;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, xcl, Buffer, BufferList, xclsourceview, componentpalette,
  propeditor, designform, xpr, Compiler;

type
  TMainForm = class(TForm)
    fcdOpen: TFileChooserDialog;
    fcdOpenProject: TFileChooserDialog;
    fcdSaveAs: TFileChooserDialog;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileOpenProject: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileSaveAll: TAction;
    actFileClose: TAction;
    actFileCloseAll: TAction;
    actFileQuit: TAction;
    actAddComponentChild: TAction;
    TopToolBox: THBox;
    AboutDlg: TAboutDialog;
    MenuBar: TMenuBar;
    PBLogo: TPixbuf;
    NB: TNoteBook;
    ProjectTV: TTreeView;
    ProjectTS: TTreeStore;
    ComponentTV: TTreeView;
    PropTable: TTable;
    EventTable: TTable;
    nbSide: TNotebook;
    npProjMan: TNotebookPage;
    npObjIns: TNotebookPage;
    procedure FileOpen(Sender: TObject);
    procedure FileOpenProject(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveUpd(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure FileSaveAsUpd(Sender: TObject);
    procedure FileSaveAll(Sender: TObject);
    procedure FileSaveAllUpd(Sender: TObject);
    procedure FileClose(Sender: TObject);
    procedure FileCloseUpd(Sender: TObject);
    procedure FileCloseAll(Sender: TObject);
    procedure FileCloseAllUpd(Sender: TObject);
    procedure FileQuit(Sender: TObject);
    procedure ProjectBuild(Sender: TObject);
    procedure ProjectCompile(Sender: TObject);
    procedure ProjectRun(Sender: TObject);
    procedure ProjectOptions(Sender: TObject);
    procedure HelpAbout(Sender: TObject);
    procedure ShowCompilerOptions(Sender: TObject);
    procedure ShowEditorOptions(Sender: TObject);
    procedure GoLeft(Sender: TObject);
    procedure GoRight(Sender: TObject);
    procedure ToggleFormCode(Sender: TObject);
    procedure ShowObjectInspector(Sender: TObject);
    procedure ShowProjectManager(Sender: TObject);
    procedure CompChanged(Sender: TObject);
    procedure RemoveComponent(Sender: TObject);
    procedure AddComponentChild(Sender: TObject);
    procedure AddComponentChildUpd(Sender: TObject);
    procedure SwitchPage(Sender: TObject; NewPage: Integer);
    procedure MainFormShow(Sender: TObject);
    procedure ProjectTVRowActivated(Sender: TObject; const Iter: TTreeIter; Column: TTreeViewColumn);
  private
    procedure PaletteClassSelected(Sender: TObject; AClass: TComponentClass);
    //
    function CurrentBuffer: TBuffer;
    function AddTree(C: TComponent; P: TTreeIter): TTreeIter;
    //--
    procedure UpdateProjectManager;
  protected
    procedure DoCloseQuery(var CanClose: Boolean); override;
  public
    LangMan: TSourceLanguagesManager;
    CompPalette: TComponentPalette;
    //--
    Buffers: TBufferList;
    //--
    CompEd: TComponentEditor;
    MyForm: TDesignForm;
    Project: TXPRProject;
    //--
    constructor Create(AOwner: TComponent); override;
    //--
    procedure SelectComp(C: TComponent);
    procedure SelectForm(B: TBuffer);
    //--
    procedure DoFileOpen(AFileName: String);
  end;

var
  MainForm: TMainForm;

implementation

uses Process,
  project_opts, compiler_opts, editor_opts,
  TxtBuffer, PasBuffer, FrmBuffer, frm_NewFile;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  LangMan := TSourceLanguagesManager.Create(Self);
  //==
  CompPalette := TComponentPalette.Create(Self);
  CompPalette.Parent := TopToolBox;
//  CompPalette.BoxExpand := True;
  CompPalette.OnClassSelected := @PaletteClassSelected;
  //==
  Buffers := TBufferList.Create(Self);
  //==
  CompEd := TComponentEditor.Create(Self);
  CompEd.PropTable := PropTable;
  CompEd.EventTable := EventTable;

  Project := TXPRProject.Create;
  //Icon := PBLogo; //Bugs on Win32!!!
end;

function TMainForm.CurrentBuffer: TBuffer;
begin
  Result := TBuffer(NB.Pages[NB.CurrentPage]);
end;

procedure TMainForm.FileNew(Sender: TObject);
var
  B: TBuffer;
begin
  B := nil;
  frmNewFile := TFrmNewFile.Create(nil);
  try
    frmNewFile.Tag := 0;
    frmNewFile.ShowModal(Self);
    case frmNewFile.Tag of
      1: B := TPasBuffer.Create(Self);
      2: B := TFrmBuffer.Create(Self);
    end;
  finally
    frmNewFile.Free;
  end;
  if Assigned(B) then
  begin
    B.New;
    B.Parent := NB;
  end;
end;

procedure TMainForm.FileOpen(Sender: TObject);
begin
  if fcdOpen.Execute = -3 then
    DoFileOpen(fcdOpen.FileName);
end;

procedure TMainForm.DoFileOpen(AFileName: String);
var
  B: TBuffer;
  ext: String;
begin
  ext := LowerCase(ExtractFileExt(AFileName));

  if ext = '.frm' then
    B := Buffers.GetByFileName(ChangeFileExt(AFileName, '.pas'))
  else
    B := Buffers.GetByFileName(AFileName);

  if not Assigned(B) then
  begin
    if FileExists(ChangeFileExt(AFileName, '.frm')) then
    begin
      AFileName := ChangeFileExt(AFileName, '.frm');
      ext := LowerCase(ExtractFileExt(AFileName));
    end;
   
    if (ext = '.xpr') then
    begin
      Project.Load(AFileName);
      UpdateProjectManager;
      exit;
    end
    else if (ext = '.pas') or (ext = '.pp') or (ext = '.inc') or (ext = '.dpr') or (ext = '.p') then
      B := TPasBuffer.Create(Self)
    else if (ext = '.frm') then
      B := TFrmBuffer.Create(Self)
    else
      B := TTxtBuffer.Create(Self);
    Buffers.Add(B);
    B.Open(AFileName);
    B.Parent := NB;
  end;
  NB.CurrentPage := NB.PageNum(B);
// You don't need this, Changing current page to the only one, does not call OnSwitchPage,
// but when you add the first page, its called.
//  if not Assigned(MyForm) then
//    SelectForm(B); 
end;

procedure TMainForm.FileOpenProject(Sender: TObject);
begin
  if (fcdOpenProject.Execute = -3) and FileExists(fcdOpenProject.FileName) then
  begin
    Project.Load(fcdOpenProject.FileName);
    UpdateProjectManager;
  end;
end;

procedure TMainForm.MainFormShow(Sender: TObject);
begin
  Icon := PBLogo;
end;

procedure TMainForm.FileSave(Sender: TObject);
begin
  CurrentBuffer.Save;
end;

procedure TMainForm.FileSaveUpd(Sender: TObject);
begin
  actFileSave.Sensitive := CurrentBuffer <> nil;
end;

procedure TMainForm.FileSaveAs(Sender: TObject);
begin
  if fcdSaveAs.Execute = -3 then
    CurrentBuffer.SaveAs(fcdSaveAs.FileName);
end;

procedure TMainForm.FileSaveAsUpd(Sender: TObject);
begin
  actFileSaveAs.Sensitive := CurrentBuffer <> nil;
end;

procedure TMainForm.FileSaveAll(Sender: TObject);
begin
  //TODO
end;

procedure TMainForm.FileSaveAllUpd(Sender: TObject);
begin
  //TODO
end;

procedure TMainForm.FileClose(Sender: TObject);
begin
  CurrentBuffer.Close;
end;

procedure TMainForm.FileCloseUpd(Sender: TObject);
begin
  actFileClose.Sensitive := CurrentBuffer <> nil;
end;

procedure TMainForm.FileCloseAll(Sender: TObject);
var
  lOK: Boolean;
begin
  lOK := True;
  while lOK and (CurrentBuffer <> nil) do
    lOK := CurrentBuffer.Close;
  if not lOK then
    Abort;
end;

procedure TMainForm.FileCloseAllUpd(Sender: TObject);
begin
  actFileCloseAll.Sensitive := CurrentBuffer <> nil;
end;

procedure TMainForm.DoCloseQuery(var CanClose: Boolean);
begin
  inherited;
  try
    actFileCloseAll.Execute;
  except
    on EAbort do
      CanClose := False;
  end;
end;

procedure TMainForm.FileQuit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ProjectBuild(Sender: TObject);
var
  C: TCompiler;
begin
  C := TCompiler.Create;
  try
    C.Compile(Project, True, False);
  finally
    C.Free;
  end;
end;

procedure TMainForm.ProjectCompile(Sender: TObject);
var
  C: TCompiler;
begin
  C := TCompiler.Create;
  try
    C.Compile(Project, False, False);
  finally
    C.Free;
  end;
end;

procedure TMainForm.ProjectRun(Sender: TObject);
var
  C: TCompiler;
  OK: Boolean;
  P: TProcess;
begin

  if Project.NeedsToCompile then
  begin
    C := TCompiler.Create;
    try
      OK := C.Compile(Project, False, True);
    finally
      C.Free;
    end;
  end
  else
    OK := True;

  if OK then
  begin
    P := TProcess.Create(nil);
    try
      P.CommandLine := GetCurrentDir+'/'+Project.ProjectExeFile;
      P.Execute;
      while (not Application.Terminated) and P.Running do
        Application.ProcessMessages(True);
      if P.Running then // Means Application Terminated
        P.Terminate(0);
    finally
      P.Free;
    end;
  end;
end;

procedure TMainForm.ProjectOptions(Sender: TObject);
var
  Frm: TFrmProjectOpts;
begin
  Frm := TFrmProjectOpts.Create(nil);
  try
    Frm.Project := Project;
    Frm.ShowModal(Self);
    Project.Save;
  finally
    Frm.Free;
  end;
end;

procedure TMainForm.HelpAbout(Sender: TObject);
var
  SL: TStringList;
  S: TStream;
begin
  try
    S := TResourceStream.Create('STR', 'license');
    try
      SL := TStringList.Create;
      SL.LoadFromStream(S);
      //AboutDlg.WrapLicense := True;
      AboutDlg.License := SL.Text;
      SL.Free;
    finally
      S.Free;
    end;
  except
    // Does not show License Button on About Dialog :D
  end;
  AboutDlg.Execute;
end;

procedure TMainForm.ShowCompilerOptions(Sender: TObject);
begin
  FrmCompilerOpts := TFrmCompilerOpts.Create(nil);
  try
    FrmCompilerOpts.ShowModal(Self);
  finally
    FrmCompilerOpts.Free;
  end;
end;

procedure TMainForm.ShowEditorOptions(Sender: TObject);
begin
  FrmEditorOpts := TFrmEditorOpts.Create(nil);
  try
    FrmEditorOpts.ShowModal(Self);
  finally
    FrmEditorOpts.Free;
  end;
end;

procedure TMainForm.SwitchPage(Sender: TObject; NewPage: Integer);
begin
  SelectForm(TBuffer(NB.Pages[NewPage]));
end;

procedure TMainForm.GoLeft(Sender: TObject);
begin
  NB.PrevPage;
end;

procedure TMainForm.GoRight(Sender: TObject);
begin
  NB.NextPage;
end;

procedure TMainForm.UpdateProjectManager;
var
  It, P, PP: TTreeIter;
  I: Integer;
begin
  ProjectTS.Clear;
  if Project.Name <> '' then
  begin
    ProjectTS.Append(PP);
    ProjectTS.SetStringValue(PP, 0, Project.Name);
    ProjectTS.SetPointerValue(PP, 1, Project);

    for I := 0 to Project.Sources.UnitCount -1 do
    begin
      ProjectTS.Append(It, PP);
      ProjectTS.SetStringValue(It, 0, Project.Sources.Units[I].Name);
      ProjectTS.SetPointerValue(It, 1, Project.Sources.Units[I]);
    end;

    for I := 0 to Project.Sources.IncludeCount -1 do
    begin
      ProjectTS.Append(It, PP);
      ProjectTS.SetStringValue(It, 0, Project.Sources.Includes[I].FileName);
      ProjectTS.SetPointerValue(It, 1, Project.Sources.Includes[I]);
    end;

    ProjectTS.Append(P, PP);
    ProjectTS.SetStringValue(P, 0, 'Resources');
    ProjectTS.SetPointerValue(P, 1, Project.Sources.Units[I]);

    for I := 0 to Project.Resources.Count -1 do
    begin
      ProjectTS.Append(It, P);
      ProjectTS.SetStringValue(It, 0, Project.Resources[I].RType + ' \ ' + Project.Resources[I].RName);
      ProjectTS.SetPointerValue(It, 1, Project.Resources[I]);
    end;
  end;
end;

procedure TMainForm.ProjectTVRowActivated(Sender: TObject; const Iter: TTreeIter; Column: TTreeViewColumn);
var
  Item: TXPRCustom;
begin
  Item := TXPRCustom(ProjectTS.GetPointerValue(Iter, 1));
  if Item is TXPRSource then
    DoFileOpen(TXPRSource(Item).FileName)
  else if Item is TXPRProject then
    DoFileOpen(TXPRProject(Item).ProjectFile)
  else if Item is TXPRResources then
    DoFileOpen(TXPRResources(Item).FileName);
end;


procedure TMainForm.CompChanged(Sender: TObject);
var
  It: TTreeIter;
  C: TComponent;
begin
  if ComponentTV.GetSelected(It) then
  begin
    C := TComponent(ComponentTV.Model.GetPointerValue(It, 1));

    CompEd.Component := C;
  end;
end;

function TMainForm.AddTree(C: TComponent; P: TTreeIter): TTreeIter;
var
  It: TTreeIter;
  I: Integer;
  S: String;
begin
  if C.Name <> '' then
    S := C.Name + ': ' + C.ClassName
  else
    S := '['+C.ClassName+']';

  TTreeStore(ComponentTV.Model).Append(It, P);
  ComponentTV.Model.SetStringValue(It, 0, S);
  ComponentTV.Model.SetPointerValue(It, 1, C);

  if C is TControl then
    for I := 0 to TControl(C).ControlCount -1 do
      AddTree(TControl(C).Controls[I], It);

  Result := It;
end;

procedure TMainForm.PaletteClassSelected(Sender: TObject; AClass: TComponentClass);
var
  C: TComponent;
  It: TTreeIter;
begin
  if Assigned(CompEd.Component) then
  begin
    C := AClass.Create(MyForm);
    try
      if (C is TControl) then
      begin
        TControl(C).Parent := TControl(CompEd.Component);
        ComponentTV.GetSelected(It);
      end
      else if (C is TCustomAction) and (CompEd.Component is TActionList) then
      begin
        TCustomAction(C).ActionList := TActionList(CompEd.Component);
        ComponentTV.GetSelected(It);
      end
      else
        ComponentTV.Model.GetIterFirst(It);
    except
      C.Free;
      raise;
    end;

    It := AddTree(C, It);
    ComponentTV.ExpandTo(It);
    ComponentTV.SelectIter(It);
  end;
end;

procedure TMainForm.SelectComp(C: TComponent);

  function FindIter(var A: TTreeIter): boolean;
  var
    B: TTreeIter;
  begin
    Result := False;
    if TComponent(ComponentTV.Model.GetPointerValue(A, 1)) = C then
      exit(True);
    if ComponentTV.Model.IterChildren(B, A) then
      if FindIter(B) then
      begin
        A := B;
        exit(True);
      end;
    if ComponentTV.Model.IterNext(A) then
      Result := FindIter(A);
  end;

var
  It: TTreeIter;
begin
  if Assigned(C) and Assigned(ComponentTV.Model) and ComponentTV.Model.GetIterFirst(It) and FindIter(It) then
  begin
    ComponentTV.ExpandTo(It);
    ComponentTV.SelectIter(It);

    CompEd.Component := C;
  end;
end;

procedure TMainForm.RemoveComponent(Sender: TObject);
var
  It: TTreeIter;
  C,P: TComponent;
begin
  if ComponentTV.GetSelected(It) then
  begin
    C := TComponent(ComponentTV.Model.GetPointerValue(It, 1));
    if C <> MyForm then
    begin
      if C is TControl then
      begin
        P := TControl(C).Parent;
        TControl(C).Parent := nil;
      end
      else
        P := MyForm;
  
      TTreeStore(ComponentTV.Model).Remove(It);
      SelectComp(P);
      C.Free;
    end;
  end;
end;

procedure TMainForm.AddComponentChild(Sender: TObject);
var
  It: TTreeIter;
  C: TComponent;
begin
  if ComponentTV.GetSelected(It) then
  begin
    C := TComponent(ComponentTV.Model.GetPointerValue(It, 1));
    if C is TNotebook then
      PaletteClassSelected(Self, TNotebookPage)
    else if C is TActionList then
      PaletteClassSelected(Self, TAction)
    else if C is TToolBar then
      PaletteClassSelected(Self, TToolItem)
    else if (C is TMenuBar) or (C is TMenuItem) then
      PaletteClassSelected(Self, TMenuItem)
  end;
end;

procedure TMainForm.AddComponentChildUpd(Sender: TObject);
var
  It: TTreeIter;
  C: TComponent;
begin
  if ComponentTV.GetSelected(It) then
  begin
    C := TComponent(ComponentTV.Model.GetPointerValue(It, 1));
    actAddComponentChild.Sensitive := (C is TNotebook) or (C is TActionList) or (C is TToolBar) or (C is TMenuBar) or (C is TMenuItem);
    if (C is TNotebook) then
      actAddComponentChild.Caption := 'Add TNotebookPage'
    else if (C is TActionList) then
      actAddComponentChild.Caption := 'Add TAction'
    else if (C is TToolBar) then
      actAddComponentChild.Caption := 'Add TToolItem'
    else if (C is TMenuBar) or (C is TMenuItem) then
      actAddComponentChild.Caption := 'Add TMenuItem'
    else
      actAddComponentChild.Caption := 'Add child...'
  end
  else
  begin
    actAddComponentChild.Sensitive := False;
    actAddComponentChild.Caption := 'Add child...'
  end;
end;

procedure TMainForm.SelectForm(B: TBuffer);
var
  FB: TFrmBuffer;
begin
  if Assigned(B) and (B is TFrmBuffer) then
  begin
    FB := TFrmBuffer(B);
    MyForm := FB.MyForm;
    ComponentTV.Model := FB.ComponentTS;
  end
  else
  begin
    MyForm := nil;
    ComponentTV.Model := nil;
    CompED.Component := nil;
  end;
end;

procedure TMainForm.ToggleFormCode(Sender: TObject);
var
  FB: TFrmBuffer;
begin
  if CurrentBuffer is TFrmBuffer then
  begin
    FB := TFrmBuffer(CurrentBuffer);
    if FB.TextBox.Visible then
    begin
      FB.TextBox.Visible := False;
      FB.FormBox.Visible := True;
    end
    else
    begin
      FB.FormBox.Visible := False;
      FB.TextBox.Visible := True;
      FB.Edt.GrabFocus;
    end;
  end;
end;

procedure TMainForm.ShowObjectInspector(Sender: TObject);
begin
  nbSide.CurrentPage := nbSide.PageNum(npObjIns);
  ComponentTV.GrabFocus;
end;

procedure TMainForm.ShowProjectManager(Sender: TObject);
begin
  nbSide.CurrentPage := nbSide.PageNum(npProjMan);
  ProjectTV.GrabFocus;
end;

end.
