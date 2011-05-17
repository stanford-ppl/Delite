inline Face::Face(const FaceData & data_)
: MeshObject(data_) {}
inline Face::Face(const FaceData & data, const FaceInfo & info_)
: MeshObject(data) {}

#define DIR (static_cast<CellSide>( ((unsigned int) data.id) >> FLIP_DIRECTION_SHIFT))


inline Cell Face::outside() const {
    return Cell(CellData(data.mesh,data.mesh->data.ftoc.values[ID()][DIR]));
}
inline Cell Face::inside() const {
    return Cell(CellData(data.mesh,data.mesh->data.ftoc.values[ID()][otherCellSide(DIR)]));
}
static CellSide cell_side(const Face & f, const Cell & c) {
    if(f.outside() == c)
        return OUTSIDE;
    else {
        assert(f.inside() == c);
        return INSIDE;
    }
}
inline Cell Face::opposite(const Cell & c) const {
    CellSide side = otherCellSide(cell_side(*this,c));
    return Cell(CellData(data.mesh,data.mesh->data.ftoc.values[ID()][side]));
}
inline void Face::towards(const Cell & c) {
    CellSide side = cell_side(*this,c);
    data.id = ID() | side << FLIP_DIRECTION_SHIFT;
}


inline void Face::flip() {
  data.id ^= (1u << FLIP_DIRECTION_SHIFT);
}

typedef CRSConstIterator<Cell> Face_cell_it;
typedef CRSDirectedIterator<Edge> Face_edge_it;
typedef CRSDirectedIterator<Vertex> Face_vertex_it;

SUBCLASS_SIZEDMESHSET(Face,vertex_set,Face_vertex_it)
SUBCLASS_SIZEDMESHSET(Face,edge_set,Face_edge_it)
SUBCLASS_SIZEDMESHSET(Face,facet_set,facet_it)
SUBCLASS_SIZEDMESHSET(Face,cell_set,Face_cell_it);

inline Face::vertex_set Face::vertices() const {
    return vertex_set(Face_vertex_it(data.mesh,data.mesh->data.ftov,ID(),CCW));
}

inline Face::vertex_set Face::verticesCCW() const {
    return verticesCCW(outside());
}
inline Face::vertex_set Face::verticesCCW(const Cell & c) const {
    if(data.mesh->data.ftoc.values[ID()][OUTSIDE] == c.ID()) {
        return vertex_set(Face_vertex_it(data.mesh,data.mesh->data.ftov,ID(),CCW));
    } else {
        assert(data.mesh->data.ftoc.values[ID()][INSIDE] == c.ID());
        return vertex_set(Face_vertex_it(data.mesh,data.mesh->data.ftov,ID(),CW));
    }
}
inline Face::vertex_set Face::verticesCW() const {
    return verticesCW(outside());
}

inline Face::vertex_set Face::verticesCW(const Cell & c) const {
    if(data.mesh->data.ftoc.values[ID()][INSIDE] == c.ID()) {
        return vertex_set(Face_vertex_it(data.mesh,data.mesh->data.ftov,ID(),CCW));
    } else {
        assert(data.mesh->data.ftoc.values[ID()][OUTSIDE] == c.ID());
        return vertex_set(Face_vertex_it(data.mesh,data.mesh->data.ftov,ID(),CW));
    }
}


inline Face::edge_set Face::edges() const {
    return edge_set(Face_edge_it(data.mesh,data.mesh->data.ftoe,ID(),CCW));
}

inline Face::edge_set Face::edgesCW() const {
    return edgesCW(outside());
}

inline Face::edge_set Face::edgesCW(const Cell & c) const {
    if(data.mesh->data.ftoc.values[ID()][INSIDE] == c.ID()) {
        return edge_set(Face_edge_it(data.mesh,data.mesh->data.ftoe,ID(),CCW));
    } else {
        assert(data.mesh->data.ftoc.values[ID()][OUTSIDE] == c.ID());
        return edge_set(Face_edge_it(data.mesh,data.mesh->data.ftoe,ID(),CW));
    }
}

inline Face::edge_set Face::edgesCCW() const {
    return edgesCCW(outside());
}

inline Face::edge_set Face::edgesCCW(const Cell & c) const {
    if(data.mesh->data.ftoc.values[ID()][OUTSIDE] == c.ID()) {
        return edge_set(Face_edge_it(data.mesh,data.mesh->data.ftoe,ID(),CCW));
    } else {
        assert(data.mesh->data.ftoc.values[ID()][INSIDE] == c.ID());
        return edge_set(Face_edge_it(data.mesh,data.mesh->data.ftoe,ID(),CW));
    }
}


inline Face::cell_set Face::cells() const {
    return cell_set(Face_cell_it(data.mesh,data.mesh->data.ftoc,ID()));
}

#undef DIR
