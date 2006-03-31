unit designform;

{$H+}
{$MODE ObjFpc}

interface

uses Classes, SysUtils, xcl;

type
  TDesignForm = class(TAlignment)
  private
    FTitle: String;
    FWidth: Integer;
    FHeight: Integer;
    FResizable: Boolean;
    FDecorated: Boolean;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function DesignShowProp(AName: ShortString): Boolean; override;
  published
    property Title: string read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Resizable: Boolean read FResizable write FResizable default True;
    property Decorated: Boolean read FDecorated write FDecorated default True;
  end;

implementation

{ TDesignForm }

constructor TDesignForm.Create(AOwner: TComponent);
begin
  inherited;
  //== TAlignment Stuff
  SetAll(0, 0, False, False);
  //== (fake) TForm Stuff
  FResizable := True;
  FDecorated := True;
  SetDesigning(True); 
end;

procedure TDesignForm.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  if Root = Self then
    for I := 0 to ComponentCount - 1 do
      if not Components[I].HasParent
        then Proc(Components[I]);
  inherited GetChildren(Proc, Root);
end;

function TDesignForm.DesignShowProp(AName: ShortString): Boolean;
begin
  if (AName = 'XAlign') or (AName = 'YAlign') or (AName = 'XScale') or (AName = 'YScale') or
   (AName = 'PopupMenu') then // This is a workaround, There's a bug when load PopupMenu property (how to fix it?)
    Result := False
  else
    Result := inherited;
end;

end.
