#pragma once 

#include <CGAL/Simple_cartesian.h>
#include <CGAL/Gmpq.h>

using Rational = CGAL::Gmpq;
using Simple_kernel = CGAL::Simple_cartesian<Rational>;

// 2D Geometry
using Point_2 = Simple_kernel::Point_2;
using Segment_2 = Simple_kernel::Segment_2;