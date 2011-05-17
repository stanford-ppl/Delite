#ifndef BOUNDARY_TABLE_H_
#define BOUNDARY_TABLE_H_

#include <vector>
#include <string>
#include <limits>
#include <iostream>
#include <map>
#include <list>
#include <cstring>
#include <sstream>
#include "MeshIO/LisztFileWriter.h"

namespace MeshIO {

class BoundaryTable {
private:
    typedef std::hash_map<int,BoundarySet> boundary_map_t;
    boundary_map_t boundary_sets;
    typedef std::pair<int,std::string> boundary_name;
    typedef std::vector<boundary_name> boundary_names_t;
    boundary_names_t boundary_names;
    typedef std::map< std::string, std::vector<std::string> > boundary_agg_t;
    boundary_agg_t boundary_aggs;  // Handles both user-defined names and Fluent types
    std::list<std::string> aggregate_order;
    int max_id;  // For internal ids used for aggregate boundary sets
    bool finalized;
    static const lsize_t INVALID_ID = static_cast<lsize_t>(-1);

public:
    BoundaryTable() : max_id(std::numeric_limits<int>::min()), finalized(false) { }

    void parseArgs(int argc, char** argv) {
        if (argc > 0) {
            assert(argv != NULL);
            parseUserBoundaries(argc, argv);
        }
    }

    void addBoundaryName(const std::string& bname, int id) {
        assert(!finalized);
        boundary_names.push_back(boundary_name(id, bname));
    }

    void addBoundaryToAggregate(const std::string& aggregate,
            const std::string& boundary) {
        assert(!finalized);
        if (boundary_aggs.find(aggregate) == boundary_aggs.end()) {
            // Never seen this aggregate name before, add to order
            // Push front allows type names defined within file to be processed
            // before user types
            aggregate_order.push_front(aggregate);
        }
        boundary_aggs[aggregate].push_back(boundary);
    }

    void addBoundary(int id, IOElemType typ,id_t start, id_t end) {
        assert(!finalized);
        // std::cout << "Adding " << id << "/" << typ << ": "
        //     << start << "-" << end << std::endl;
        BoundarySet bs;
        bs.type = typ;
        bs.start = start;
        bs.end = end;
        bs.name_string = 0;
        if(boundary_sets.find(id) != boundary_sets.end()) {
            printf("WARNING: boundary set id %d is used twice in the file, keeping the newer set\n",id);
        }
        boundary_sets[id] = bs;
        max_id = std::max(id, max_id);
    }

    lsize_t addBoundary(const std::string& name, IOElemType typ,
            id_t start, id_t end) {
        boundary_names.push_back(boundary_name(max_id + 1, name));
        addBoundary(max_id + 1, typ, start, end);
        // Since IDs are determined by order in boundary_names, offset 
        // of newly added name becomes its ID
        return boundary_names.size() - 1;
    }

    void writeBoundarySetTable(LisztFileWriter& writer) {
        assert(finalized);
        writer.header.nBoundaries = 0;
        //first create the name table, a list of null terminated strings, and
        //assign the boundary sets to point to them
        for(std::vector<boundary_name>::iterator it = boundary_names.begin();
            it != boundary_names.end(); it++) {
            boundary_map_t::iterator bs = boundary_sets.find(it->first);
            if(bs != boundary_sets.end()) {
                bs->second.name_string = writer.currentPosition();
                writer.writeString(it->second);
                writer.header.nBoundaries++;
            } else {
                std::cout << "WARNING!!!! SKIPPING "
                    << it - boundary_names.begin() << std::endl;
            }
        }
        
        //now write the boundary sets
        writer.header.boundary_set_table = writer.currentPosition();
        unsigned int bs_count = 0;
        //only write named boundaries for which we have found corresponding ids
        for(std::vector<boundary_name>::iterator it = boundary_names.begin();
            it != boundary_names.end(); it++) {
            boundary_map_t::iterator bs = boundary_sets.find(it->first);
            if(bs != boundary_sets.end()) {
                BoundarySet * bsd = &bs->second;
                writer.writeObject(bsd);
                bs_count++;
            } else {
                std::cout << "WARNING!!!! SKIPPING "
                    << it - boundary_names.begin() << std::endl;
            }
        }
        assert(writer.header.nBoundaries == bs_count);
    }
private:
    void eraseUnusedBoundaryNames() {
        // The left_id/right_id format used for aggregate boundaries in the file
        // requires fixing the positions of the elements in the table to calculate
        // an offset. Boundaries are written to the file in the same order as the
        // names were read in from the Fluent file. However, only those boundaries
        // and names that *both* appear are written to the file, so we can't just use
        // the boundary_names array as is, since the corresponding zone may not
        // actually appear.
        // To handle this, we preprocessing to erase unused boundary_name entries,
        // leaving exactly the names used and in exactly the position the corresponding
        // boundary_set entries will appear in the table.
        for(std::vector<boundary_name>::iterator it = boundary_names.begin();
            it != boundary_names.end(); ) {
            boundary_map_t::iterator bs = boundary_sets.find(it->first);
            if(bs == boundary_sets.end()) {
                it = boundary_names.erase(it);
            } else {
                ++it;
            }
        }
    }

    void parseUserBoundaries(int argc, char** argv) {
        // Expected input format: "super_boundary:boundary1,boundary2,..."
        for (int i = 0; i < argc; ++i) {
            char* arg = argv[i];
            char* colon_pos = strchr(arg, ':');
            if (colon_pos == NULL) {
                std::cout << "WARNING: Ignoring invalid boundary arg '" << arg << "'"
                    << std::endl;
                continue;
            }
            // TODO(mbarrien): Verify proper formatting of strings
            // TODO(mbarrien): Verify non-duplication of names of boundaries
            // TODO(mbarrien): Verify non-duplication within list of sets
            std::string bname(arg, colon_pos - arg);
            // std::cout << "User boundary name '" << bname << "'" << std::endl;
            if (boundary_aggs.find(bname) == boundary_aggs.end()) {
                aggregate_order.push_back(bname);
            }
            std::vector<std::string>& sub_boundaries = boundary_aggs[bname];
            char* saveptr;
            const char* token = strtok_r(colon_pos + 1, ",", &saveptr);
            while (token != NULL) {
				// std::cout << "Token: '" << token << "'" << std::endl;
                sub_boundaries.push_back(token);
                token = strtok_r(NULL, ",", &saveptr);
            }
        }
    }

    /*
    void printBoundaryIDs() const {
        std::cout << "---------------" << std::endl;
        for (boundary_names_t::const_iterator iter = boundary_names.begin();
                iter != boundary_names.end(); ++iter) {
            std::cout << "[" << iter-boundary_names.begin() << "] " << iter->first
                << "/" << iter->second << std::endl;
        }
        std::cout << "---------------" << std::endl;
    }
    */

    lsize_t boundaryID(const std::string& name, IOElemType* type = NULL) const {
        // printBoundaryIDs();
        for (boundary_names_t::const_iterator iter = boundary_names.begin();
                iter != boundary_names.end(); ++iter) {
            if (iter->second == name) {
                if (type) {
                    *type = boundary_sets.find(iter->first)->second.type;
                }
                return iter - boundary_names.begin();
            }
        }
        return INVALID_ID;
    }

    static bool parseBoundaryLiteral(const std::string& s, IOElemType& type,
            id_t& start, id_t& end) {
        // TODO(mbarrien): Add these unit tests
        // * Correct vertex, edge, face, cell
        // * Invalid string (no colon)
        // * Invalid elem type "foo:"
        // * Correct elem type, no number "vertex:"
        // * Correct type single number "vertex:15"
        // * Correct type, single number with dash but no second number "vertex:15-"
        // * Correct type, not a number "vertex:foo"
        // * Correct type, second not a number "vertex:15-foo"
        // Check for presence of ":" before doing more expensive string comparisons
        size_t colon_pos = s.find(':');
        if (colon_pos == std::string::npos) {
            return false;
        }
        if (s.find("vertex:") == 0) { type = VERTEX_T; }
        else if (s.find("edge:") == 0) { type = EDGE_T; }
        else if (s.find("face:") == 0) { type = FACE_T; }
        else if (s.find("cell:") == 0) { type = CELL_T; }
        else { std::cout << "Does not begin with elem!" << std::endl; return false; }

        // Head of string is correct, now parse numbers.
        // Parse "123-456"
        // TODO(mbarrien): Check negative numbers and within range
        size_t dash_pos = s.find('-', colon_pos + 1);
        std::istringstream ss_begin(s.substr(colon_pos + 1, dash_pos - (colon_pos + 1)));
        ss_begin >> start;
        if (ss_begin.rdstate() & std::ios::failbit) { return false; }

        std::istringstream ss_end(s.substr(dash_pos + 1));
        ss_end >> end;
        if (ss_end.rdstate() & std::ios::failbit) { return false; }
        // Since end is exclusive, but literals are inclusive, add 1.
        end += 1;
        return true;
    }

    std::string mangleBoundaryName(const std::string& bname,
            const std::string& sub_name) const {
        return std::string("agg:") + bname + "_" + sub_name;
    }

    std::string mangleBoundaryName(const std::string& bname, unsigned int id) const {
        char id_str[20];
        snprintf(id_str, sizeof(id_str), "%u", id);
        return mangleBoundaryName(bname, id_str);
    }

    lsize_t addAggregate(const std::string& name, IOElemType typ,
            lsize_t left, lsize_t right) {
        return addBoundary(name, static_cast<IOElemType>(typ | AGG_FLAG),
                static_cast<id_t>(left), static_cast<id_t>(right));
    }

    void addAggregateBoundaries() {
        typedef std::vector<std::string> sub_boundary_vec;
        for (std::list<std::string>::iterator iter = aggregate_order.begin();
                iter != aggregate_order.end(); ++iter) {
            const std::string& bname = *iter;
            sub_boundary_vec& sub_boundaries = boundary_aggs[bname];
            // std::cout << "Processing aggregate '" << bname << "' with "
			// // 	<< sub_boundaries.size() << " children" << std::endl;
            // Two passes through the sub_boundaries; first to remove invalid
            // boundaries and add boundary literals, second to actually create
            // the entries.
            IOElemType boundary_type = TYPE_SIZE;
            for (sub_boundary_vec::iterator sub_iter = sub_boundaries.begin();
                    sub_iter != sub_boundaries.end();) {
				// std::cout << "Sub-boundary '" << *sub_iter << "'" << std::endl;
                id_t start, end;
                IOElemType sub_type;
                if (boundaryID(*sub_iter, &sub_type) != INVALID_ID) {
					// Valid boundary, let it fall through to the checks
                } else if (parseBoundaryLiteral(*sub_iter, sub_type, start, end)) { 
                    // Add boundary literal, with a fixed naming convention
                    // Replace literal name in list with mangled name
                    *sub_iter = mangleBoundaryName(bname, *sub_iter);
                    addBoundary(*sub_iter, sub_type, start, end);
                } else {
                    std::cout << "WARNING: In boundary '" << bname
                        << "', could not find sub-boundary '" << *sub_iter
                        << "', ignoring sub-boundary" << std::endl;
                    sub_iter = sub_boundaries.erase(sub_iter);
                    continue;
                }

                // Verify all types of sub-boundaries are the same as the type of
                // the very first valid one in the list.
                sub_type = static_cast<IOElemType>(sub_type & ~AGG_FLAG);
                // TODO(mbarrien): Check that sub_boundaries.begin() is correct
                // even after performing erase on the first element.
                if (sub_iter == sub_boundaries.begin()) {
                    boundary_type = sub_type;
                } else if (sub_type != boundary_type) {
                    std::cout << "WARNING: Boundary '" << bname
                        << "' appears to be of type " << boundary_type
                        << " but sub-boundary '" << *sub_iter << "' is of type "
                        << sub_type << ", ignoring entire boundary." << std::endl;
                    sub_boundaries.clear();
                    break;
                }
				++sub_iter;
            }

            if (sub_boundaries.empty()) {
                // String was formatted like "boundary:". May have become empty
                // after removing.
                std::cout << "WARNING: Boundary '" << bname
                    << "' does not list other boundary names. Ignoring." << std::endl;
                continue;
            }
			assert(boundary_type != TYPE_SIZE);
            // Build "tree" of sub-boundaries (actually a left-skewed tree,
            // which in reality is almost like a linked list). The left child
            // acts as a "next" pointer, while the right child points to an actual
            // entry. Instead of null-termination, we just put the first 2 entries
            // as the left/right child of the final element.
            // The only case where we "null-terminate" is when the sub-boundary list
            // is of length 1.
            if (sub_boundaries.size() == 1) {
                // Just copy the individual ID we have
                boundary_names_t::const_iterator iter = boundary_names.begin();
                while (iter != boundary_names.end() &&
                        iter->second != sub_boundaries.front()) { ++iter; }
                assert(iter != boundary_names.end());
                const BoundarySet& set = boundary_sets[iter->first];
                addBoundary(bname, set.type, set.start, set.end);
                continue;
            }

            // NOTE: Every call to addAggregate will automatically increment max_id.
            sub_boundary_vec::const_iterator sub_iter = sub_boundaries.begin();
            // If length 1, prev_id stays the special ID, otherwise combine the
            // first 2 sub-boundaries into a single tree.
            lsize_t prev_id = boundaryID(*sub_iter++);
            while (sub_iter != sub_boundaries.end()) {
                // Use bname as top level tree name if last one, otherwise, use
                // artifically generated name
                lsize_t cur_id = boundaryID(*sub_iter++);
                prev_id = addAggregate(sub_iter == sub_boundaries.end() ? bname :
                        mangleBoundaryName(bname, sub_iter-sub_boundaries.begin()-1),
                        boundary_type, prev_id, cur_id);
            }
        }
    }

public:
    void finalizeBoundarySetTable() {
        assert(!finalized);
        eraseUnusedBoundaryNames();
        addAggregateBoundaries();
        finalized = true;
    }
};
} // namespace MeshIO
#endif // BOUNDARY_TABLE_H_
