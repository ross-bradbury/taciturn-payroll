using System;
using System.Drawing;
using System.Drawing.Printing;

namespace PayrollForWindows
{
	public class TextPrintDocument : PrintDocument
	{
		private string _text;
		/// <summary>
		/// Gets or sets the text that is to be printed.
		/// </summary>
		public string Text
		{
			get { return _text; }
            set
            {
                _text = value ?? string.Empty;
                _currentPosition = 0;
            }
		}

		private Font _font;
		/// <summary>
		/// Property to hold the font the users wishes to use
		/// </summary>
		/// <value></value>
		/// <returns></returns>
		/// <remarks></remarks>
		public Font Font
		{
			get { return _font; }
			set { _font = value ?? new Font("Courier New", 10); }
		}

		/// <summary>
		/// Hold the current character we're currently dealing with.
		/// </summary>
		private int _currentPosition;

		/// <summary>
		/// Empty constructor
		/// </summary>
		/// <remarks></remarks>
		public TextPrintDocument()
			: this("")
		{
		}

		/// <summary>
		/// Constructor to initialize our printing object
		/// and the text it's supposed to be printing
		/// </summary>
		/// <param name="text">Text that will be printed</param>
		/// <remarks></remarks>
		public TextPrintDocument(string text)
		{
			//Set the file stream
			//Set our Text property value
			this.Text = text ?? "";
			this.Font = new Font("Courier New", 10);
		}

		protected override void OnBeginPrint(PrintEventArgs e)
		{
			_currentPosition = 0;
			base.OnBeginPrint(e);
		}

		protected override void OnPrintPage(PrintPageEventArgs e)
		{
			// Run base code
			base.OnPrintPage(e);

			//Set print area size and margins

			var margins = this.DefaultPageSettings.Margins;
			var paperSize = this.DefaultPageSettings.PaperSize;

			int printHeight = paperSize.Height - margins.Top - margins.Bottom;
			int printWidth = paperSize.Width - margins.Left - margins.Right;
			int leftMargin = margins.Left; //X
			int rightMargin = margins.Top; //Y

			//Check if the user selected to print in Landscape mode
			//if they did then we need to swap height/width parameters
			if (this.DefaultPageSettings.Landscape)
			{
				int tmp = printHeight;
				printHeight = printWidth;
				printWidth = tmp;
			}

			//Now we need to determine the total number of lines
			//we're going to be printing
			Int32 numLines = (int) printHeight/Font.Height;

			//Create a rectangle printing are for our document
			var printArea = new RectangleF(leftMargin, rightMargin, printWidth, printHeight);

			//Use the StringFormat class for the text layout of our document
			var format = new StringFormat(StringFormatFlags.LineLimit);

			var text = this.Text;
			var remainingText = text.Substring(_currentPosition);
			var formFeedPosition = remainingText.IndexOf((char) 12);
			if (formFeedPosition >= 0)
				remainingText = remainingText.Substring(0, formFeedPosition);

			//Fit as many characters as we can into the print area      
			Int32 lines;
			Int32 chars;
			e.Graphics.MeasureString(remainingText, Font,
			                         new SizeF(printWidth, printHeight),
			                         format, out chars, out lines);

			//Print the page
			e.Graphics.DrawString(remainingText, Font,
			                      Brushes.Black, printArea, format);

			//Increase current char count
			_currentPosition += chars;
			if (formFeedPosition >= 0 && chars == formFeedPosition)
				_currentPosition++;

			//Detemine if there is more text to print, if
			//there is the tell the printer there is more coming
			if (_currentPosition < text.Length)
			{
				e.HasMorePages = true;
			}
			else
			{
				e.HasMorePages = false;
				_currentPosition = 0;
			}
		}
	}
}
