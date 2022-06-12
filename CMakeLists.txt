cmake_minimum_required(VERSION 3.23)

project(computational-geometry)

find_package(CGAL CONFIG REQUIRED)

add_library(geolib INTERFACE)

target_include_directories(geolib INTERFACE geolib/)


add_executable(convexhull convexhull/convexhull.cpp)
add_executable(segmentintersection segmentintersection/segmentintersection.cpp)

target_link_libraries(convexhull PRIVATE CGAL::CGAL geolib)
target_link_libraries(segmentintersection PRIVATE CGAL::CGAL geolib)