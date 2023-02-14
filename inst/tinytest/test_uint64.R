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

ints <- c(0L, 1L, 123L)
x <- as.uint64(ints)

expect_equal(
  as.integer(as.character(x)),
  ints
)

chars <- c( "123456789123456789", "18446744073709551614" )
x <- as.uint64( chars )
expect_equal( as.character(x), chars )
