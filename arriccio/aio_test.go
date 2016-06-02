package main

import (
	"testing"
	)

func TestCompareVersion(t *testing.T) {
	if compareVersion("1.0", "1.2") <= 0 {
		t.Error("fail 1.0 < 1.2")
	}
	if compareVersion("1.0", "1.0") != 0 {
		t.Error("version compare 1.0 == 1.0")
	}
	if compareVersion("1.2.3", "1.2.5") <= 0 {
		t.Error("version compare 1.2.3 < 1.2.5")
	}
	if compareVersion("1.2", "1.2.1") <= 0 {
		t.Error("version compare 1.2 < 1.2.1")
	}
	if compareVersion("1.2", "1.2.0") != 0 {
		t.Error("version compare 1.2 == 1.2.0")
	}
}

func TestCheckVersionConstraint(t *testing.T) {
	if checkVersionAgainstConstraint("1.2.1", "1.2.1") != true {
		t.Error("version constraint 1.2.1 1.2.1")
	}
	if checkVersionAgainstConstraint("1.2.1", "1.2.2") != false {
		t.Error("version constraint 1.2.1 1.2.2")
	}
	if checkVersionAgainstConstraint("1.2.1", "1.2.0") != false {
		t.Error("version constraint 1.2.1 1.2.0")
	}
	if checkVersionAgainstConstraint("1.2.3", "1.2") != true {
		t.Error("version constraint 1.2.3 1.2")
	}
	if checkVersionAgainstConstraint("1.2.0", "1.2") != true {
		t.Error("version constraint 1.2.0 1.2")
	}
	if checkVersionAgainstConstraint("1.3.0", "1.2") != true {
		t.Error("version constraint 1.3.0 1.2")
	}
	if checkVersionAgainstConstraint("1.1.0", "1.2") != false {
		t.Error("version constraint 1.1.0 1.2")
	}
	if checkVersionAgainstConstraint("2.1.0", "1.2") != false {
		t.Error("version constraint 2.1.0 1.2")
	}
}