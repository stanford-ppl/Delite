
inline Vertex::Vertex(const VertexData & data) : MeshObject(data) {}

SUBCLASS_SIZEDMESHSET(Vertex,vertex_set,vertex_it)
SUBCLASS_SIZEDMESHSET(Vertex,edge_set,edge_it)
SUBCLASS_SIZEDMESHSET(Vertex,face_set,face_it)
SUBCLASS_SIZEDMESHSET(Vertex,cell_set,cell_it)


inline Vertex::vertex_set Vertex::vertices() const {
    return vertex_set(vertex_it(data.mesh,data.mesh->data.vtov,ID()));
}


inline Vertex::edge_set Vertex::edges() const {
    return edge_set(edge_it(data.mesh,data.mesh->data.vtoe,ID()));
}


inline Vertex::face_set Vertex::faces() const {
    return face_set(face_it(data.mesh,data.mesh->data.vtof,ID()));
}


inline Vertex::cell_set Vertex::cells() const {
    return cell_set(cell_it(data.mesh,data.mesh->data.vtoc,ID()));
}


