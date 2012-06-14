

inline Edge::Edge(const EdgeData & data_)
: MeshObject(data_){}
inline Edge::Edge(const EdgeData & data, const EdgeInfo & dir)
: MeshObject(data){}

#define DIR (static_cast<VertexSide>(((unsigned int) data.id) >> FLIP_DIRECTION_SHIFT))

inline Vertex Edge::head() const {
    return Vertex(VertexData(data.mesh,data.mesh->data.etov.values[ID()][DIR]));
}
inline Vertex Edge::tail() const {
    return Vertex(VertexData(data.mesh,data.mesh->data.etov.values[ID()][otherVertexSide(DIR)]));
}
static VertexSide vertex_side(const Edge & e, const Vertex & v) {
    if(e.head() == v)
        return HEAD;
    else if(e.tail() == v)
        return TAIL;
    else {
        assert(false);
        return HEAD;
    }
}

inline Vertex Edge::opposite(const Vertex & v) const {
    VertexSide side = otherVertexSide(vertex_side(*this,v));
    return Vertex(VertexData(data.mesh,data.mesh->data.etov.values[ID()][side]));
}
inline void Edge::towards(const Vertex & v) {
    VertexSide side = vertex_side(*this,v);
    data.id = ID() | (side << FLIP_DIRECTION_SHIFT);
}


inline void Edge::flip() {
    data.id ^= (1u << FLIP_DIRECTION_SHIFT);
}

typedef CRSConstIterator<Vertex> Edge_vertex_it;
typedef CRSDirectedIterator<Face> Edge_face_it;
typedef CRSDirectedIterator<Cell> Edge_cell_it;

SUBCLASS_SIZEDMESHSET(Edge,vertex_set,Edge_vertex_it)
SUBCLASS_SIZEDMESHSET(Edge,face_set,Edge_face_it)
SUBCLASS_SIZEDMESHSET(Edge,facet_set,facet_it)
SUBCLASS_SIZEDMESHSET(Edge,cell_set,Edge_cell_it)



inline Edge::face_set Edge::facesCCW() const {
    return facesCCW(head());
}
// TODO(mbarrien): Combine CW/CCW into one implementation
inline Edge::face_set Edge::facesCCW(const Vertex & v) const {
    if(data.mesh->data.etov.values[ID()][HEAD] == v.ID()) {
        return face_set(Edge_face_it(data.mesh,data.mesh->data.etof,ID(),CCW));
    }
    else {
        assert(data.mesh->data.etov.values[ID()][TAIL] == v.ID());
        return face_set(Edge_face_it(data.mesh,data.mesh->data.etof,ID(),CW));
    }
}


inline Edge::face_set Edge::facesCW() const {
    return facesCW(head());
}

inline Edge::face_set Edge::facesCW(const Vertex & v) const {
    if(data.mesh->data.etov.values[ID()][TAIL] == v.ID()) {
        return face_set(Edge_face_it(data.mesh,data.mesh->data.etof,ID(),CCW));
    }
    else {
        assert(data.mesh->data.etov.values[ID()][HEAD] == v.ID());
        return face_set(Edge_face_it(data.mesh,data.mesh->data.etof,ID(),CW));
    }
}

inline Edge::cell_set Edge::cellsCCW() const {
    return cellsCCW(head());
}

inline Edge::cell_set Edge::cellsCCW(const Vertex & v) const {
    if(data.mesh->data.etov.values[ID()][HEAD] == v.ID()) {
        return cell_set(Edge_cell_it(data.mesh,data.mesh->data.etoc,ID(),CCW));
    }
    else {
        assert(data.mesh->data.etov.values[ID()][TAIL] == v.ID());
        return cell_set(Edge_cell_it(data.mesh,data.mesh->data.etoc,ID(),CW));
    }
}

inline Edge::cell_set Edge::cellsCW() const {
    return cellsCW(head());
}


inline Edge::cell_set Edge::cellsCW(const Vertex & v) const {
    if(data.mesh->data.etov.values[ID()][TAIL] == v.ID()) {
        return cell_set(Edge_cell_it(data.mesh,data.mesh->data.etoc,ID(),CCW));
    }
    else {
        assert(data.mesh->data.etov.values[ID()][HEAD] == v.ID());
        return cell_set(Edge_cell_it(data.mesh,data.mesh->data.etoc,ID(),CW));
    }
}
inline Edge::vertex_set Edge::vertices() const {
    return vertex_set(Edge_vertex_it(data.mesh,data.mesh->data.etov,ID()));
}


inline Edge::face_set Edge::faces() const {
    return face_set(Edge_face_it(data.mesh,data.mesh->data.etof,ID(),CCW));
}

inline Edge::cell_set Edge::cells() const {
    return cell_set(Edge_cell_it(data.mesh,data.mesh->data.etoc,ID(),CCW));
}

#undef DIR
