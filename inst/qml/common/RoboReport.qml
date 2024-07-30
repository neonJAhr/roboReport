//
// Copyright (C) 2013-2024 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Section
{
	title:		"Automatic Report"
	info: qsTr("This is currently based on the BarPlots file and the ANOVA Common ")

	CheckBox { name: "roboReport"; 	label: qsTr("Enable Report")}

	Group
	{
		CheckBox 
		{ 	name: "textAssumptions";	
			label: qsTr("Include Assumption Testing"); 
			checked: true; 
			id: checkAssumptions
		}
		
 		Group
 		{
 			title: qsTr("Assumption Checks")
 			visible: checkAssumptions.checked
 			CheckBox { name: "textNormalityTest";	label: qsTr("Normality") }
 			CheckBox
 			{
 				name: "textEqualityOfVariancesTest";	label: qsTr("Equality of variances"); 
 				RadioButtonGroup
 				{
 					name: "textEqualityOfVariancesTestType"
 					RadioButton { value: "brownForsythe";	label: qsTr("Brown-Forsythe"); checked: true }
 					RadioButton { value: "levene";			label: qsTr("Levene's") }
 				}
 			}
 		}
	}
		
	RadioButtonGroup
	{
		name: "text_alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton { value: "twoSided";	label: qsTr("Group 1 ≠ Group 2"); checked: true	}
		RadioButton { value: "greater";	label: qsTr("Group 1 > Group 2")					}
		RadioButton { value: "less"; label: qsTr("Group 1 < Group 2")						}
	}
	
	RadioButtonGroup
	{
		name: "verbosityLevels";
		title: qsTr("Verbosity")
		RadioButton { value: "verbLow";	label: qsTr("Low"); checked: true	}
		RadioButton { value: "verbMedium";	label: qsTr("Medium")			}
		RadioButton { value: "verbHigh"; label: qsTr("High")				}
	}
}
