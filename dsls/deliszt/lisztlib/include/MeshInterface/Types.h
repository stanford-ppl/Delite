/*
 * Types.h
 *
 *  Created on: Aug 10, 2009
 *      Author: mbarrien
 */


namespace __MESH_IMPL_NAMESPACE_PRIVATE__ { 
enum Rotation { CW = -1, CCW = 1 };
enum VertexSide { HEAD = 0, TAIL = 1 };
enum CellSide { OUTSIDE = 0, INSIDE = 1 };
enum FaceSide { LEFT = 0, RIGHT = 1 };  // Used in 2-D meshes

inline Rotation otherRotation(Rotation rot) {
    return static_cast<Rotation>(-static_cast<size_t>(rot));
}
inline VertexSide otherVertexSide(VertexSide dir) {
    return static_cast<VertexSide>(!static_cast<size_t>(dir));
}
inline CellSide otherCellSide(CellSide side) {
    return static_cast<CellSide>(!static_cast<size_t>(side));
}
inline FaceSide otherFaceSide(FaceSide side) {
    return static_cast<FaceSide>(!static_cast<size_t>(side));
}
inline CellSide operator^(CellSide side, CellSide other) {  // Changes side if side and other are mismatched
    return static_cast<CellSide>(static_cast<unsigned int>(side) ^ static_cast<unsigned int>(other));
}
inline CellSide& operator^=(CellSide& side, CellSide other) {  // Changes side if side and other are mismatched
    side = side ^ other;
    return side;
}
inline VertexSide operator^(VertexSide side, VertexSide other) {
    return static_cast<VertexSide>(static_cast<unsigned int>(side) ^ static_cast<unsigned int>(other));
}
inline VertexSide& operator^=(VertexSide& side, VertexSide other) {
    side = side ^ other;
    return side;
}
}
