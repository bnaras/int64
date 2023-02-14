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


ints <- c(-122L, 0L, 1L, 122L)
x <- as.int64(ints)

expect_equal(
  as.integer(as.character(x)),
  ints
)
expect_equal( as.character(x+1L), as.character(ints+1L) )
expect_equal( as.character(x-1L), as.character(ints-1L) )
expect_equal( as.character(x*x), as.character(ints*ints) )
expect_equal( as.character(x/2L), as.character(as.integer(ints/2L)) )

expect_equal( x > 0L, ints > 0L )
expect_equal( x < 0L, ints < 0L )
expect_equal( x == 0L, ints == 0L )
expect_equal( x != 0L, ints != 0L )
expect_equal( x <= 0L, ints <= 0L )
expect_equal( x >= 0L, ints >= 0L )

expect_equal( range(x), as.int64(c(-122L, 122L)) )
expect_equal( min(x), as.int64(-122L) )
expect_equal( max(x), as.int64(122L) )
expect_equal( prod(x), as.int64(as.integer(prod(ints))) )
expect_equal( sum(x), as.int64(as.integer(sum(ints))) )
expect_equal( any(x), any(ints) )
expect_equal( all(x), all(ints) )

chars <- c( "-9223372036854775807", "9223372036854775807" )
x <- as.int64( chars )
expect_equal( as.character(x), chars )
