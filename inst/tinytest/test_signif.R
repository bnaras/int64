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
x <- as.int64( c( "12345", "12345", "12345" ) )
expect_equal( 
  signif( x, c(2,3,7) ),
  as.int64( c("12000", "12300", "12345") )
)

x <- as.uint64( c( "12345", "12345", "12345" ) )
expect_equal( 
  signif( x, c(2,3,7) ),
  as.uint64( c("12000", "12300", "12345") )
)     
