unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, USaveLoad, UAppState, UFigures;

type

  { TCircularBuffer }

  TCircularBuffer = class
  private
    FBuffer: array[1..30] of String;
    FFirst: Integer;
    FLast: Integer;
    FCurrent: Integer;
    FSaved: Integer;
    function GetNextIndex: Integer;
    function GetPreviousIndex: Integer;
  public
    property First: Integer read FFirst write FFirst;
    property Last: Integer read FLast write Flast;
    property Saved: Integer read FSaved write FSaved;
    property Current: Integer read FCurrent write FCurrent;
    procedure Push(AString: String);
    function RollBack: String;
    function RollForward: String;
    procedure SetSaved;
    constructor Create(AFirstState: String);
  end;

  { THistory }

  THistory = class
  private
    FBuffer: TCircularBuffer;
  public
    function CanBack: Boolean;
    function CanForward: Boolean;
    procedure AddState;
    procedure SetPreviousState;
    procedure SetNextState;
    procedure SetSaved;
    function IsModified: Boolean;
    procedure SetCaption;
    constructor Create;
    destructor Destroy; override;
  end;

var
  History: THistory;

implementation

{ TCircularBuffer }

function TCircularBuffer.GetNextIndex: Integer;
begin
  if FCurrent = High(FBuffer) then
    Result := Low(FBuffer)
  else
    Result := FCurrent + 1;
end;

function TCircularBuffer.GetPreviousIndex: Integer;
begin
  if FCurrent = Low(FBuffer) then
    Result := High(FBuffer)
  else
    Result := FCurrent - 1;
end;

procedure TCircularBuffer.Push(AString: String);
begin
  FCurrent := GetNextIndex;
  FBuffer[FCurrent] := AString;
  Last := Current;
  if Last = First then begin
    First := GetNextIndex;
    Saved := Low(FBuffer) - 1;//вне значений индексов буффера
  end;
end;

function TCircularBuffer.RollBack: String;
begin
  if FCurrent <> First then
    FCurrent := GetPreviousIndex;
  Result := FBuffer[FCurrent]
end;

function TCircularBuffer.RollForward: String;
begin
  if FCurrent <> Last then
    FCurrent := GetNextIndex;
  Result := FBuffer[FCurrent];
end;

procedure TCircularBuffer.SetSaved;
begin
  FSaved := FCurrent;
end;

constructor TCircularBuffer.Create(AFirstState: String);
begin
  First := Low(FBuffer);
  FBuffer[First] := AFirstState;
  Last := First;
  FCurrent := First;
  FSaved := First;
end;

{ THistory }

function THistory.CanBack: Boolean;
begin
  Result := FBuffer.Current <> FBuffer.First;
end;

function THistory.CanForward: Boolean;
begin
  Result := FBuffer.Current <> FBuffer.Last;
end;

procedure THistory.AddState;
begin
  FBuffer.Push(GetSaveStr(Figures));
  SetCaption;
end;

procedure THistory.SetPreviousState;
begin
  StringLoad(FBuffer.RollBack);
  SetCaption;
end;

procedure THistory.SetNextState;
begin
  StringLoad(FBuffer.RollForward);
  SetCaption;
end;

procedure THistory.SetSaved;
begin
  FBuffer.SetSaved;
  SetCaption;
end;

procedure THistory.SetCaption;
begin
  UpdateCaption(IsModified);
end;

function THistory.IsModified: Boolean;
begin
  Result := FBuffer.Current <> FBuffer.Saved;
end;

constructor THistory.Create;
begin
  Inherited Create;
  FBuffer := TCircularBuffer.Create(GetSaveStr(Figures));
  SetCaption;
end;

destructor THistory.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

end.

