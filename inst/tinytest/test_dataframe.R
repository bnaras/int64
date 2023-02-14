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
df <- data.frame( a = 1:4 )
df$b <- as.int64( 1:4 )
df$c <- as.uint64( 1:4 )

expect_equal( df$b[3:4], df$b[1:2] + 2L )
expect_equal( df$c[3:4], df$c[1:2] + 2L )
