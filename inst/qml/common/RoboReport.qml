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
	}
    Group {
        id: dataGroup
        title: qsTr("Data Assumption Checks")
        CheckBox { 
            id: textNormalityTest
            name: "textNormalityTest"
            label: qsTr("Normality")
            checked: normalityTest.checked
            onCheckedChanged: normalityTest.checked = checked
        }
        CheckBox {
            id: textEqualityOfVariancesTest
            name: "textEqualityOfVariancesTest"
            label: qsTr("Equality of variances")
            checked: equalityOfVariancesTest.checked
            onCheckedChanged: equalityOfVariancesTest.checked = checked
            RadioButtonGroup {
                id: textEqualityOfVariancesTestType
                name: "textEqualityOfVariancesTestType"
                RadioButton { 
                    id: textBrownForsythe
                    value: "textBrownForsythe"
                    label: qsTr("Brown-Forsythe")
                    checked: brownForsythe.checked
                    onCheckedChanged: brownForsythe.checked = checked
                }
                RadioButton { 
                    id: textLevene
                    value: "textLevene"
                    label: qsTr("Levene's")
                    checked: levene.checked
                    onCheckedChanged: levene.checked = checked
                }
            }
        }
	}
		
	RadioButtonGroup
	{
		name: "textAlternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton { value: "textTwoSided";	
			label: qsTr("Group 1 ≠ Group 2"); 
			checked: twoSided.checked
			onCheckedChanged: twoSided.checked = checked
			}
		RadioButton { 
			value: "textGreater";	
			label: qsTr("Group 1 > Group 2")
			checked: greater.checked
			onCheckedChanged: greater.checked = checked
			}
		RadioButton { 
			value: "textLess"; 
			label: qsTr("Group 1 < Group 2")
			checked: less.checked
			onCheckedChanged: less.checked = checked
			}
	}
	
	RadioButtonGroup
	{
		name: "verbosityLevels";
		title: qsTr("Verbosity")
		RadioButton { value: "verbLow";	label: qsTr("Low")						}
		RadioButton { value: "verbMedium";	label: qsTr("Medium"); checked: true}
		RadioButton { value: "verbHigh"; label: qsTr("High")					}
	}
}
