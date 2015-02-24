library("salesperson")
load("network.RData")

x = getNearestNeighbourFeatureSet(inst)

# read_scaled_network = function (path) {
#   con = file(path, open = "r")
#   on.exit(close(con))
#   tsp_inst = list()
#   tsp_inst = tspmeta:::read_tsplib_specification(con, tsp_inst)
#   if (substr(tsp_inst$TYPE, 1, 3) != "TSP") 
#     stop("Currently the only supported TYPE is: TSP!")
#   while (tspmeta:::next_line_exists(con)) {
#     line = tspmeta:::next_line(con)
#     if (line == "NODE_COORD_SECTION") {
#       tsp_inst = tspmeta:::read_tsplib_node_coords(con, tsp_inst)
#     } else if (line == "EDGE_WEIGHT_SECTION") {
#       tsp_inst = tspmeta:::read_tsplib_edge_weights(con, tsp_inst)
#     } else if (line == "DISPLAY_DATA_SECTION") {
#       tsp_inst = tspmeta:::read_tsplib_display_data(con, tsp_inst)
#     } else if (line == "FIXED_EDGES_SECTION") {
#       tsp_inst = tspmeta:::read_tsplib_fixed_edges(con, tsp_inst)
#     } else if (line == "") {
#       next
#     } else if (line == "EOF") {
#       break
#     } else {
#       stop("Unhandled data section '", line, "' in TSPLIB-file.")
#     }
#   }
#   netw = makeNetwork(as.matrix(tsp_inst$NODE_COORDS))
#   rescaleNetwork(netw)
# }
# 
# inst = read_tsplib_instance("lu980.tsp")
# netw = makeNetwork(as.matrix(inst$coords))
# inst = rescaleNetwork(netw)
