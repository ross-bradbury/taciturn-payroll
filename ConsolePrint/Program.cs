using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Printing;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace PayrollForWindows
{
	class Program
	{
		static void Main(string[] args)
		{
			using (var printer = new TextPrintDocument())
			{
				printer.PrintController = new StandardPrintController();
				printer.DefaultPageSettings.Margins.Left = 75;
				printer.DefaultPageSettings.Margins.Right = 75;
				printer.Text = File.ReadAllText(args[0]);
				printer.Print();
			}
		}
	}
}
