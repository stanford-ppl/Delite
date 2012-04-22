inline Cell::Cell(const CellData & data) : MeshObject(data) {}

SUBCLASS_SIZEDMESHSET(Cell,vertex_set,vertex_it)
SUBCLASS_SIZEDMESHSET(Cell,edge_set,edge_it)
SUBCLASS_SIZEDMESHSET(Cell,face_set,face_it)
SUBCLASS_SIZEDMESHSET(Cell,cell_set,cell_it)

inline Cell::vertex_set Cell::vertices() const {
    return vertex_set(vertex_it(data.mesh,data.mesh->data.ctov,ID()));
}

inline Cell::edge_set Cell::edges() const {
    return edge_set(edge_it(data.mesh,data.mesh->data.ctoe,ID()));
}

inline Cell::face_set Cell::faces() const {
    return face_set(face_it(data.mesh,data.mesh->data.ctof,ID()));
}

inline Cell::cell_set Cell::cells() const {
    return cell_set(cell_it(data.mesh,data.mesh->data.ctoc,ID()));
}

inline id_type Cell::_set_id() const {
    return get_set_id() ;
}


