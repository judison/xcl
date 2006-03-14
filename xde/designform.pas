unit designform;

{$H+}
{$MODE ObjFpc}

interface

uses Classes, SysUtils, xcl;

type
  TDesignForm = class(TFixed)
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


end.