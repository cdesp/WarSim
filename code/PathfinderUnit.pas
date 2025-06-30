unit PathfinderUnit;

interface

uses
  classes, System.Generics.Collections, System.SysUtils;

type
  TTile = record
    X, Y: Integer;
    class operator Add(const A, B: TTile): TTile;
    class operator Subtract(const A, B: TTile): TTile;
  end;

//  TTileArray= array[0..5] of TTile;//to keep the adjacent tiles
  TTileArray= array of TTile;//to keep the adjacent tiles

  TCubeCoord = record
    X, Y, Z: Integer;  // x + y + z = 0 always
  end;

  TNode = class
    Tile: TTile;
    G, H: Integer;
    Parent: TNode;
    constructor Create(aX, aY: Integer);
  end;

  TIsTilePassableEvent = function(X, Y: Integer): Boolean of object;
  TGetSpeedEvent = function(X, Y: Integer): Integer of object;

  TPathfinder = class
  private
    FStart, FEnd: TTile;

  public
    class var
      OnIsTilePassable: TIsTilePassableEvent;
      OnGetSpeed: TGetSpeedEvent;
    class function GetHexDirection(StartPos, TargetPos: TTile): Integer; static;
    class function GetHexTilesAtDistance(const TilePosition: TTile;
      const Distance: Integer): TTileArray; static;
    class function GetHexAdjTiles(Pos: TTile): TTileArray; static;
    class function HexFacingDifference(UnitFacing: Integer; UnitTilePos,
      EnemyTilePos: TTile): Integer; static;
    constructor Create(aStartX, aStartY, aEndX, aEndY: Integer);
    function FindPath: TList;
  end;

function Tile(X, Y:Integer):TTile;

implementation
uses windows,math;

type
  T2DIntArray = array[0..5, 0..1] of Integer;
  P2DIntArray = ^T2DIntArray;
const

  // Even rows (0, 2, 4, etc.)
  HexEvenROffsets: T2DIntArray = (
    (0, +1),    // NorthEast
    (-1, 0),    // East
    (0, -1),    // SouthEast
    (+1, -1),   // SouthWest
    (+1, 0),    // West
    (+1, +1)    // NorthWest
  );

  // Odd rows (1, 3, 5, etc.)
  HexOddROffsets: T2DIntArray = (
    (-1, +1),   // NorthEast
    (-1, 0),    // East
    (-1, -1),   // SouthEast
    (0, -1),    // SouthWest
    (+1, 0),    // West
    (0, +1)     // NorthWest
  );




function Tile(X, Y:Integer):TTile;
Begin
  Result.X:=X;
  Result.Y:=Y;
End;

class operator TTile.Add(const A, B: TTile): TTile;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TTile.Subtract(const A, B: TTile): TTile;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

constructor TNode.Create(aX, aY: Integer);
begin
  Tile.X := aX;
  Tile.Y := aY;
  G := 0;
  H := 0;
  Parent := nil;
end;

constructor TPathfinder.Create(aStartX, aStartY, aEndX, aEndY: Integer);
begin
  FStart.X := aStartX;
  FStart.Y := aStartY;
  FEnd.X := aEndX;
  FEnd.Y := aEndY;
end;

function Vector3Integer(X, Y, Z: Integer): TCubeCoord;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

class function TPathfinder.GetHexTilesAtDistance(
  const TilePosition: TTile;
  const Distance: Integer
): TTileArray;
const
  // Directions 0..5 same as your system, reused
  DirCount = 6;
var
  Q, R, DQ, DR: Integer;
  I, Step, Dir: Integer;
  Current: TTile;
  Directions: T2DIntArray;
  ResultList: TTileArray;
begin
  if Distance = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := TilePosition;
    Exit;
  end;

  SetLength(ResultList, 0);

  // Starting position: move 'Distance' steps in direction 4 (West)
  Current := TilePosition;
  for Step := 1 to Distance do
  begin
    if (Current.Y mod 2) = 0 then
      Directions := HexEvenROffsets
    else
      Directions := HexOddROffsets;

    Current.X := Current.X + Directions[4, 0];
    Current.Y := Current.Y + Directions[4, 1];
  end;

  // Now walk around the ring
  for Dir := 0 to DirCount - 1 do
  begin
    for Step := 1 to Distance do
    begin
      if (Current.Y mod 2) = 0 then
        Directions := HexEvenROffsets
      else
        Directions := HexOddROffsets;

      Current.X := Current.X + Directions[Dir, 0];
      Current.Y := Current.Y + Directions[Dir, 1];

      // Append to result
      SetLength(ResultList, Length(ResultList) + 1);
      ResultList[High(ResultList)] := Current;
    end;
  end;

  Result := ResultList;
end;



class function TPathfinder.GetHexAdjTiles(Pos: TTile): TTileArray;
var
  RowParity: Boolean;
  Offsets: T2DIntArray;
  OffsetX, OffsetY: Integer;
  I: Integer;

begin
  SetLength(Result, 6);
  RowParity := (Pos.Y mod 2) = 0; // Even-r layout → row parity based on Y
  if RowParity then
    Offsets := HexEvenROffsets
  else
    Offsets := HexOddROffsets;

  for I := 0 to High(Offsets) do
  begin
     result[i].X:=Pos.X+Offsets[i,0];
     result[i].Y:=Pos.Y+Offsets[i,1];
  end;

end;

class function TPathfinder.GetHexDirection(StartPos, TargetPos: TTile): Integer;
var
  RowParity: Boolean;
  Offsets: T2DIntArray;
  OffsetX, OffsetY: Integer;
  I: Integer;
begin
  OffsetX := TargetPos.X - StartPos.X;
  OffsetY := TargetPos.Y - StartPos.Y;

  RowParity := (StartPos.Y mod 2) = 0; // Even-r layout → row parity based on Y
  if RowParity then
    Offsets := HexEvenROffsets
  else
    Offsets := HexOddROffsets;

  for I := 0 to High(Offsets) do
  begin
    if (Offsets[I][0] = OffsetX) and (Offsets[I][1] = OffsetY) then
      Exit(I);
  end;

  // Not a direct neighbor — you can raise an exception or return -1
  Result := -1;
end;

class function TPathfinder.HexFacingDifference(
  UnitFacing: Integer;
  UnitTilePos, EnemyTilePos: TTile
): Integer;
var
  DirectionToEnemy, Diff: Integer;
begin
  DirectionToEnemy := GetHexDirection(UnitTilePos, EnemyTilePos);

  if DirectionToEnemy = -1 then
  begin
    // Not adjacent, you can raise an exception or decide your handling here
    Result := -1;
    Exit;
  end;

  Diff := Abs(UnitFacing - DirectionToEnemy) mod 6;

  case Diff of
    0: Result := 0;
    1, 5: Result := 1;
    2, 4: Result := 2;
    3: Result := 3;
  else
    Result := -1; // should not happen
  end;
end;


function TPathfinder.FindPath: TList;

var
  OpenList, ClosedList: TObjectList<TNode>;
  CurrentNode, Neighbour: TNode;
  NeighbourTile: TTile;
  TentativeG, SpeedCost: Integer;
  i: Integer;
  PathList: TList;
  TilePointer: ^TTile;
  Offset: P2DIntArray;

  function TileInList(List: TObjectList<TNode>; X, Y: Integer): TNode;
  var
    k: Integer;
  begin
    for k := 0 to List.Count - 1 do
      if (List[k].Tile.X = X) and (List[k].Tile.Y = Y) then
        Exit(List[k]);
    Result := nil;
  end;

  function EvenRToCube(x, y: Integer): TCubeCoord;
  var
    rx, ry, rz: Integer;
  begin
    rx := x - (y + (y and 1)) div 2;
    rz := y;
    ry := -rx - rz;
    Result := Vector3Integer(rx, ry, rz);
  end;

  function HexDistance(const A, B: TTile): Integer;
  var
    ac, bc: TCubeCoord;
  begin
    ac := EvenRToCube(A.X, A.Y);
    bc := EvenRToCube(B.X, B.Y);
    Result := (Abs(ac.X - bc.X) + Abs(ac.Y - bc.Y) + Abs(ac.Z - bc.Z)) div 2;
  end;

var totalcost,totaldist:single;

begin
  OpenList := TObjectList<TNode>.Create(False);
  ClosedList := TObjectList<TNode>.Create(False);
  PathList := TList.Create;

  try
    CurrentNode := TNode.Create(FStart.X, FStart.Y);
    CurrentNode.G := 0;
    CurrentNode.H := HexDistance(CurrentNode.Tile, FEnd);
    OpenList.Add(CurrentNode);

    while OpenList.Count > 0 do
    begin
      CurrentNode := OpenList[0];
      for i := 1 to OpenList.Count - 1 do
        if (OpenList[i].G + OpenList[i].H <
            CurrentNode.G + CurrentNode.H) then
          CurrentNode := OpenList[i];

      OpenList.Remove(CurrentNode);
      ClosedList.Add(CurrentNode);

      if (CurrentNode.Tile.X = FEnd.X) and (CurrentNode.Tile.Y = FEnd.Y) then
      begin
        totalcost:=0;     totaldist:=0;
        while CurrentNode <> nil do
        begin
          New(TilePointer);
          TilePointer^ := CurrentNode.Tile;
          PathList.Add(TilePointer);
          totalcost:=totalcost+CurrentNode.G;
          totaldist:=totaldist+CurrentNode.H;
          CurrentNode := CurrentNode.Parent;
        end;
        Result := PathList;
       // Outputdebugstring(PChar(FloatToStr(totalcost)+'-'+FloatToStr(totaldist)));
        Exit;
      end;

      if (CurrentNode.Tile.Y and 1) = 0 then
        Offset := @HexEvenROffsets
      else
        Offset := @HexOddROffsets;

      for i := 0 to 5 do
      begin
        NeighbourTile.X := CurrentNode.Tile.X + Offset^[i, 0];
        NeighbourTile.Y := CurrentNode.Tile.Y + Offset^[i, 1];

        if not Assigned(OnIsTilePassable) or not OnIsTilePassable(NeighbourTile.X, NeighbourTile.Y) then
          Continue;

        if TileInList(ClosedList, NeighbourTile.X, NeighbourTile.Y) <> nil then
          Continue;

        SpeedCost := 1;
        if Assigned(OnGetSpeed) then
          SpeedCost := OnGetSpeed(NeighbourTile.X, NeighbourTile.Y);

        TentativeG := CurrentNode.G + SpeedCost;

        Neighbour := TileInList(OpenList, NeighbourTile.X, NeighbourTile.Y);
        if Neighbour = nil then
        begin
          Neighbour := TNode.Create(NeighbourTile.X, NeighbourTile.Y);
          Neighbour.G := TentativeG;
          Neighbour.H := HexDistance(NeighbourTile, FEnd);
          Neighbour.Parent := CurrentNode;
          OpenList.Add(Neighbour);
        end
        else if TentativeG < Neighbour.G then
        begin
          Neighbour.G := TentativeG;
          Neighbour.Parent := CurrentNode;
        end;
      end;
    end;

    Result := nil;
  finally
    OpenList.Free;
    ClosedList.Free;
  end;
end;

end.

