# Copyright (C) 2011	Romain Francois
# Copyright (C) 2011	Google Inc.  All rights reserved.
#
# This file is part of int64.
#
# int64 is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# int64 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with int64.  If not, see <http://www.gnu.org/licenses/>.
x <- as.int64( c(1:4, 3L ) )
expect_equal( sort( x ), as.int64( c(1:3,3L,4L) ) )
expect_equal( sort( x, decreasing = TRUE), as.int64( c(4L,3L,3:1) ) )

x <- as.uint64( c(1:4, 3L ) )
expect_equal( sort( x ), as.uint64( c(1:3,3L,4L) ) )
expect_equal( sort( x, decreasing = TRUE), as.uint64( c(4L,3L,3:1) ) )
